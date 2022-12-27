// let css = require('./node/pkg/lightningcss_node');
// console.log(css)

// const path = require('path').join(__dirname, 'node/pkg/lightningcss_node_bg.wasm');
// const bytes = require('fs').readFileSync(path);
const url = new URL('node/pkg/lightningcss_node_bg.wasm', import.meta.url);

const env = 1;
// const values = [undefined, null, globalThis, true, false];
const scopes = [];
let values;
const references = [];
const table = new WebAssembly.Table({ initial: 3200, element: 'anyfunc' });
const decoder = new TextDecoder("utf-8", { ignoreBOM: true, fatal: true });
const encoder = new TextEncoder();
function getString(ptr, len) {
  return decoder.decode(memory().subarray(ptr, ptr + len));
}

function pushScope() {
  scopes.push([undefined, null, globalThis, true, false]);
  values = scopes[scopes.length - 1];
}

function popScope() {
  scopes.pop();
  values = scopes[scopes.length - 1];
}

pushScope();

class BufferValue {
  constructor(data, length, finalize, finalizeHint) {
    this.data = data;
    this.length = length;
    this.finalize = finalize;
    this.finalizeHint = finalizeHint;
  }
}

function strlen(ptr) {
  let buf = memory();
  let len = 0;
  while (buf[ptr] !== 0) {
    len++;
    ptr++;
  }

  return len;
}

function setPointer(ptr, value) {
  memory32()[ptr >> 2] = value;
  return 0;
}

function createValue(value, result) {
  let id = values.length;
  values.push(value);
  setPointer(result, id);
  return 0;
}

const napi = {
  __indirect_function_table: table,
  napi_create_string_utf8(env, str, length, result) {
    let s = decoder.decode(memory().subarray(str, str + length));
    return createValue(s, result);
  },
  napi_create_error(env, code, msg, result) {
    let err = new Error(values[msg]);
    err.code = values[code];
    return createValue(err, result);
  },
  napi_get_named_property(env, object, utf8Name, result) {
    let obj = values[object];
    let name = getString(utf8Name, strlen(utf8Name));
    return createValue(obj[name], result);
  },
  napi_set_property(env, object, key, value) {
    let obj = values[object];
    let name = values[key];
    let val = values[value];
    obj[name] = val;
    return 0;
  },
  napi_get_property(env, object, key, result) {
    let obj = values[object];
    let name = values[key];
    return createValue(obj[name], result);
  },
  napi_throw(env, error) {
    throw values[error];
  },
  napi_throw_error(env, code, msg) {
    let err = new Error(getString(msg, strlen(msg)));
    err.code = code;
    throw err;
  },
  napi_create_object(env, result) {
    return createValue({}, result);
  },
  napi_set_named_property(env, object, utf8Name, value) {
    let obj = values[object];
    let val = values[value];
    let name = getString(utf8Name, strlen(utf8Name));
    obj[name] = val;
    return 0;
  },
  napi_define_class() {
  },
  napi_create_reference(env, value, refcount, result) {
    let id = references.length;
    references.push({
      value,
      refcount
    });
    return setPointer(result, id);
  },
  napi_delete_reference(env, ref) {
    references[id] = undefined;
    return 0;
  },
  napi_get_reference_value(env, ref, result) {
    let reference = references[ref];
    return createValue(reference.value, result);
  },
  napi_reference_ref(env, ref, result) {
    let reference = references[ref];
    reference.refcount++;
    return setPointer(result, reference.refcount);
  },
  napi_reference_unref(env, ref, result) {
    let reference = references[ref];
    reference.refcount--;
    return setPointer(result, reference.refcount);
  },
  napi_create_function(env, utf8name, length, cb, data, result) {
    let func = function (...args) {
      pushScope();

      try {
        let fn = table.get(cb);
        let info = values.length;
        values.push({
          thisArg: this,
          args,
          data
        });

        let res = fn(env, info);
        return values[res];
      } finally {
        popScope();
      }
    };

    Object.defineProperty(func, 'name', {
      value: getString(utf8name, strlen(utf8name)),
      configurable: true
    });

    return createValue(func, result);
  },
  napi_create_threadsafe_function() { },
  napi_unref_threadsafe_function() { },
  napi_typeof(env, value, result) {
    let val = values[value];
    return setPointer(result, (() => {
      switch (typeof val) {
        case 'undefined':
          return 0;
        case 'boolean':
          return 2;
        case 'number':
          return 3;
        case 'string':
          return 4;
        case 'symbol':
          return 5;
        case 'object':
          if (val === null) {
            return 1;
          }
          return 6;
        case 'function':
          return 7;
        case 'bigint':
          return 9;
      }
    })());
  },
  napi_get_boolean(env, value, result) {
    return setPointer(result, value ? 3 : 4);
  },
  napi_get_buffer_info(env, value, data, length) {
    let buf = values[value];
    setPointer(data, buf.data);
    return setPointer(length, data.length);
  },
  napi_create_double(env, value, result) {
    return createValue(value, result);
  },
  napi_create_int32(env, value, result) {
    return createValue(value, result);
  },
  napi_create_array_with_length(env, length, result) {
    return createValue(new Array(length), result);
  },
  napi_set_element(env, object, index, value) {
    let obj = values[object];
    let val = values[value];
    obj[index] = val;
    return 0;
  },
  napi_get_null(env, result) {
    return setPointer(result, 1);
  },
  napi_create_uint32(env, value, result) {
    return createValue(value, result);
  },
  napi_get_value_bool(env, value, result) {
    let val = values[value];
    return setPointer(result, val ? 1 : 0);
  },
  napi_get_value_double(env, value, result) {
    let val = values[value];
    return setPointer(result, val);
  },
  napi_is_array(env, value, result) {
    let val = values[value];
    return setPointer(result, Array.isArray(val) ? 1 : 0);
  },
  napi_get_array_length(env, value, result) {
    let val = values[value];
    return setPointer(result, val.length);
  },
  napi_get_element(env, object, index, result) {
    let obj = values[object];
    let val = obj[index];
    return createValue(val, result);
  },
  napi_is_buffer(env, value, result) {
    let val = values[value];
    return setPointer(result, val instanceof BufferValue ? 1 : 0);
  },
  napi_get_undefined(env, result) {
    return setPointer(result, 0);
  },
  napi_call_function(env, recv, func, argc, argv, result) {
    let thisArg = values[recv];
    let fn = values[func];
    let args = new Array(argc);
    for (let i = 0; i < argc; i++) {
      args[i] = values[argv];
      argv += 4;
    }

    let res = fn.apply(thisArg, args);
    return createValue(res, result);
  },
  napi_get_and_clear_last_exception() { },
  napi_get_global() {
    return setPointer(result, 2);
  },
  napi_new_instance() { },
  napi_get_cb_info(env, cbinfo, argc, argv, thisArg, data) {
    let info = values[cbinfo];
    setPointer(argc, info.args.length);
    for (let i = 0; i < info.args.length; i++) {
      createValue(info.args[i], argv);
      argv += 4;
    }
    setPointer(thisArg, createValue(info.thisArg));
    setPointer(data, info.data);
    return 0;
  },
  napi_create_buffer(env, length, data, result) {
    return createValue(new BufferValue(data, length, null, null), result);
  },
  napi_create_external_buffer(env, length, data, finalize_cb, finalize_hint, result) {
    return createValue(new BufferValue(data, length, table.get(finalize_cb), finalize_hint), result);
  },
  napi_get_value_string_utf8(env, value, buf, bufsize, result) {
    let val = values[value];
    if (buf == 0) {
      return setPointer(result, utf8Length(val));
    }
    let res = encoder.encodeInto(val, memory().subarray(ptr, ptr + bufsize - 1));
    memory()[ptr + res.written] = 0; // null terminate
    return setPointer(result, res.written);
  },
  napi_get_property_names(env, object, result) {
    let obj = values[object];
    let properties = Object.getOwnPropertyNames(obj); // Object.keys??
    return createValue(properties, result);
  },

  trace(ptr) {
    console.trace(getString(ptr, 4));
  },
};

function utf8Length(string) {
  let len = 0;
  for (let i = 0; i < string.length; i++) {
    let c = string.charCodeAt(i);

    if (c >= 0xd800 && c <= 0xdbff && i < string.length - 1) {
      let c2 = string.charCodeAt(++i);
      if ((c2 & 0xfc00) === 0xdc00) {
        c = ((c & 0x3ff) << 10) + (c2 & 0x3ff) + 0x10000;
      } else {
        // unmatched surrogate.
        i--;
      }
    }

    if ((c & 0xffffff80) === 0) {
      len++;
    } else if ((c & 0xfffff800) === 0) {
      len += 2;
    } else if ((c & 0xffff0000) === 0) {
      len += 3;
    } else if ((c & 0xffe00000) === 0) {
      len += 4;
    }
  }
  return len;
}

// const wasmModule = new WebAssembly.Module(bytes);
// const wasmInstance = new WebAssembly.Instance(wasmModule, {
//   env: napi
// });
const { instance: wasmInstance } = await WebAssembly.instantiateStreaming(fetch(url), {
  env: napi
});

let _buf32 = new Uint32Array();
function memory32() {
  if (_buf32.byteLength === 0) {
    _buf32 = new Uint32Array(wasmInstance.exports.memory.buffer);
  }

  return _buf32;
}

let _buf = new Uint8Array();
function memory() {
  if (_buf.byteLength === 0) {
    _buf = new Uint8Array(wasmInstance.exports.memory.buffer);
  }

  return _buf;
}

let napi_exports = wasmInstance.exports.wasm_malloc(4);
napi.napi_create_object(env, napi_exports);
console.log(memory(), napi_exports, memory32()[napi_exports >> 2]);

wasmInstance.exports.init_wasm(env, memory32()[napi_exports >> 2]);
console.log('done', values)

let exportsObj = values[memory32()[napi_exports >> 2]];
console.log(exportsObj)

// let code = Buffer.from('.foo { color: yellow }');
let code = encoder.encode('.foo { color: yellow }');
let ptr = wasmInstance.exports.wasm_malloc(code.byteLength);
memory().set(code, ptr);

let buf = wasmInstance.exports.wasm_malloc(4);
// napi.napi_create_buffer(env, code.byteLength, ptr, buf);
// console.log(values[memory32()[buf >> 2]])

globalThis.table = table;

let res = exportsObj.transform({
  filename: 'test.js',
  code: new BufferValue(ptr, code.byteLength),
  minify: true,
  // sourceMap: true,
  // targets: {
  //   chrome: 95 << 16
  // }
});

console.log(res)

// let

// napi.napi_call_function(env, 0,)

