let cur_await_promise_sync;
export function await_promise_sync(promise_addr, result_addr, error_addr) {
  cur_await_promise_sync(promise_addr, result_addr, error_addr);
}

const State = {
  None: 0,
  Unwinding: 1,
  Rewinding: 2
};

// This uses Binaryen's Asyncify transform to suspend native code execution while a promise is resolving.
// That allows synchronous Rust code to call async JavaScript functions without multi-threading.
// When Rust wants to await a promise, it calls await_promise_sync, which saves the stack state and unwinds.
// That causes the bundle function to return early. If a promise has been queued, we can then await it
// and "rewind" the function back to where it was before by calling it again. This time the result of
// the promise can be returned, and the function can continue where it left off.
// See the docs in https://github.com/WebAssembly/binaryen/blob/main/src/passes/Asyncify.cpp
// The code here is also partially based on https://github.com/GoogleChromeLabs/asyncify
export function createBundleAsync(env) {
  let {instance, exports} = env;
  let {asyncify_get_state, asyncify_start_unwind, asyncify_stop_unwind, asyncify_start_rewind, asyncify_stop_rewind} = instance.exports;

  // allocate __asyncify_data
  // Stack data goes right after the initial descriptor.
  let DATA_ADDR = instance.exports.napi_wasm_malloc(8 + 4096);
  let DATA_START = DATA_ADDR + 8;
  let DATA_END = DATA_ADDR + 8 + 4096;
  new Int32Array(env.memory.buffer, DATA_ADDR).set([DATA_START, DATA_END]);

  function assertNoneState() {
    if (asyncify_get_state() !== State.None) {
      throw new Error(`Invalid async state ${asyncify_get_state()}, expected 0.`);
    }
  }

  let promise, result, error;
  cur_await_promise_sync = (promise_addr, result_addr, error_addr) => {
    let state = asyncify_get_state();
    if (state === State.Rewinding) {
      asyncify_stop_rewind();
      if (result != null) {
        env.createValue(result, result_addr);
      }
      if (error != null) {
        env.createValue(error, error_addr);
      }
      promise = result = error = null;
      return;
    }
    assertNoneState();
    promise = env.get(promise_addr);
    asyncify_start_unwind(DATA_ADDR);
  };

  return async function bundleAsync(options) {
    assertNoneState();
    let res = exports.bundle(options);
    while (asyncify_get_state() === State.Unwinding) {
      asyncify_stop_unwind();
      try {
        result = await promise;
      } catch (err) {
        error = err;
      }
      assertNoneState();
      asyncify_start_rewind(DATA_ADDR);
      res = exports.bundle(options);
    }

    assertNoneState();
    return res;
  };
}
