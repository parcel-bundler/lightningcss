import { Environment, napi } from 'napi-wasm';
import { await_promise_sync, createBundleAsync } from './async.mjs';

let wasm, initPromise, bundleAsyncInternal;

export default async function init(input) {
  if (wasm) return;
  if (initPromise) {
    await initPromise;
    return;
  }

  input = input ?? new URL('lightningcss_node.wasm', import.meta.url);
  if (typeof input === 'string' || (typeof Request === 'function' && input instanceof Request) || (typeof URL === 'function' && input instanceof URL)) {
    input = fetchOrReadFromFs(input);
  }

  let env;
  initPromise = input
    .then(input => load(input, {
      env: {
        ...napi,
        await_promise_sync,
        __getrandom_v03_custom: (ptr, len) => {
          let buf = env.memory.subarray(ptr, ptr + len);
          crypto.getRandomValues(buf);
        },
      }
    }))
    .then(({instance}) => {
      instance.exports.register_module();
      env = new Environment(instance);
      bundleAsyncInternal = createBundleAsync(env);
      wasm = env.exports;
    });

  await initPromise;
}

export function transform(options) {
  return wrap(wasm.transform, options);
}

export function transformStyleAttribute(options) {
  return wrap(wasm.transformStyleAttribute, options);
}

export function bundle(options) {
  return wrap(wasm.bundle, options);
}

export function bundleAsync(options) {
  return wrap(bundleAsyncInternal, options);
}

function wrap(call, options) {
  if (typeof options.visitor === 'function') {
    let deps = [];
    options.visitor = options.visitor({
      addDependency(dep) {
        deps.push(dep);
      }
    });

    let result = call(options);
    if (result instanceof Promise) {
      result = result.then(res => {
        if (deps.length) {
          res.dependencies ??= [];
          res.dependencies.push(...deps);
        }
        return res;
      });
    } else if (deps.length) {
      result.dependencies ??= [];
      result.dependencies.push(...deps);
    }
    return result;
  } else {
    return call(options);
  }
}

export { browserslistToTargets } from './browserslistToTargets.js';
export { Features } from './flags.js';
export { composeVisitors } from './composeVisitors.js';

async function load(module, imports) {
  if (typeof Response === 'function' && module instanceof Response) {
    if (typeof WebAssembly.instantiateStreaming === 'function') {
      try {
        return await WebAssembly.instantiateStreaming(module, imports);
      } catch (e) {
        if (module.headers.get('Content-Type') != 'application/wasm') {
          console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n", e);
        } else {
          throw e;
        }
      }
    }

    const bytes = await module.arrayBuffer();
    return await WebAssembly.instantiate(bytes, imports);
  } else {
    const instance = await WebAssembly.instantiate(module, imports);
    if (instance instanceof WebAssembly.Instance) {
      return { instance, module };
    } else {
      return instance;
    }
  }
}

async function fetchOrReadFromFs(inputPath) {
  try {
    const fs = await import('fs');
    return fs.readFileSync(inputPath);
  } catch {
    return fetch(inputPath);
  }
};
