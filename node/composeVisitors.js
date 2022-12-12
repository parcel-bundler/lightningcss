/**
 * Composes multiple visitor objects into a single one.
 * @param {import('./index').Visitor[]} visitors 
 * @return {import('./index').Visitor}
 */
function composeVisitors(visitors) {
  if (visitors.length === 1) {
    return visitors[0];
  }

  /** @type import('./index').Visitor */
  let res = {};
  composeObjects(res, visitors, 'Rule', v => v.type);
  composeObjects(res, visitors, 'Property', v => v.property === 'custom' ? v.value.name : v.property);
  composeSimpleFunctions(res, visitors, 'Url');
  composeSimpleFunctions(res, visitors, 'Color');
  composeSimpleFunctions(res, visitors, 'Image');
  composeSimpleFunctions(res, visitors, 'Length');
  composeSimpleFunctions(res, visitors, 'Angle');
  composeSimpleFunctions(res, visitors, 'Ratio');
  composeSimpleFunctions(res, visitors, 'Resolution');
  composeSimpleFunctions(res, visitors, 'Time');
  composeSimpleFunctions(res, visitors, 'CustomIdent');
  composeSimpleFunctions(res, visitors, 'DashedIdent');
  composeArrayFunctions(res, visitors, 'MediaQuery');
  composeSimpleFunctions(res, visitors, 'SupportsCondition');
  composeArrayFunctions(res, visitors, 'Selector');
  composeTokenVisitors(res, visitors, 'Token', 'token');
  composeTokenVisitors(res, visitors, 'Function', 'function');
  composeTokenVisitors(res, visitors, 'Variable', 'var');
  return res;
}

module.exports = composeVisitors;

function extractObjectsOrFunctions(visitors, key) {
  let values = [];
  let hasFunction = false;
  let allKeys = new Set();
  for (let visitor of visitors) {
    let v = visitor[key];
    if (v) {
      if (typeof v === 'function') {
        hasFunction = true;
      } else {
        for (let key in v) {
          allKeys.add(key);
        }
      }
      values.push(v);
    }
  }
  return [values, hasFunction, allKeys];
}

function composeObjects(res, visitors, key, getType) {
  let [values, hasFunction, allKeys] = extractObjectsOrFunctions(visitors, key);
  if (values.length === 0) {
    return;
  }

  if (values.length === 1) {
    res[key] = values[0];
    return;
  }

  let f = createObjectVisitor(visitors, key, getType);
  if (hasFunction) {
    res[key] = f;
  } else {
    let v = {};
    for (let k of allKeys) {
      v[k] = f;
    }
    res[key] = v;
  }
}

function createObjectVisitor(visitors, key, getType) {
  let seen = new Bitset(visitors.length);
  return arg => {
    let arr = [arg];
    let mutated = false;
    seen.clear();
    for (let i = 0; i < arr.length; i++) {
      // For each value, call all visitors. If a visitor returns a new value,
      // we start over, but skip the visitor that generated the value or saw
      // it before (to avoid cycles). This way, visitors can be composed in any order. 
      for (let v = 0; v < visitors.length;) {
        if (seen.get(v)) {
          v++;
          continue;
        }

        let item = arr[i];
        let visitor = visitors[v];
        let f = visitor[key]?.[getType(item)];
        if (!f) {
          v++;
          continue;
        }

        let res = f(item);
        if (Array.isArray(res)) {
          if (res.length === 0) {
            arr.splice(i, 1);
          } else if (res.length === 1) {
            arr[i] = res[0];
          } else {
            arr.splice(i, 1, ...res);
          }
          mutated = true;
          seen.set(v);
          v = 0;
        } else if (res) {
          arr[i] = res;
          mutated = true;
          seen.set(v);
          v = 0;
        } else {
          v++;
        }
      }
    }

    if (!mutated) {
      return;
    }

    return arr.length === 1 ? arr[0] : arr;
  };
}

function composeTokenVisitors(res, visitors, key, type) {
  let [values, hasFunction, allKeys] = extractObjectsOrFunctions(visitors, key);
  if (values.length === 0) {
    return;
  }

  if (values.length === 1) {
    res[key] = values[0];
    return;
  }

  let f = createTokenVisitor(visitors, type);
  if (hasFunction) {
    res[key] = f;
  } else {
    let v = {};
    for (let key of allKeys) {
      v[key] = f;
    }
    res[key] = v;
  }
}

/**
 * @param {import('./index').Visitor[]} visitors 
 * @param {string} type 
 */
function createTokenVisitor(visitors, type) {
  return arg => {
    let arr = [{ type, value: arg }];
    let mutated = false;
    for (let visitor of visitors) {
      for (let i = 0; i < arr.length;) {
        let item = arr[i];
        let f;
        switch (item.type) {
          case 'token':
            f = visitor.Token;
            if (typeof f === 'object') {
              f = f[item.value.type];
            }
            break;
          case 'function':
            f = visitor.Function;
            if (typeof f === 'object') {
              f = f[item.value.name];
            }
            break;
          case 'variable':
            f = visitor.Variable;
            break;
          case 'color':
            f = visitor.Color;
            break;
          case 'url':
            f = visitor.Url;
            break;
          case 'length':
            f = visitor.Length;
            break;
          case 'angle':
            f = visitor.Angle;
            break;
          case 'time':
            f = visitor.Time;
            break;
          case 'resolution':
            f = visitor.Resolution;
            break;
          case 'dashed-ident':
            f = visitor.DashedIdent;
            break;
        }

        if (typeof f !== 'function') {
          i++;
          continue;
        }

        let res = f(item.value);
        switch (item.type) {
          case 'color':
          case 'url':
          case 'length':
          case 'angle':
          case 'time':
          case 'resolution':
          case 'dashed-ident':
            if (Array.isArray(res)) {
              res = res.map(value => ({ type: item.type, value }))
            } else if (res) {
              res = { type: item.type, value: res };
            }
            break;
        }

        if (Array.isArray(res)) {
          if (res.length === 0) {
            arr.splice(i, 1);
          } else if (res.length === 1) {
            arr[i++] = res[0];
          } else {
            arr.splice(i, 1, ...res);
            i += res.length;
          }
          mutated = true;
        } else if (res) {
          arr[i++] = res;
          mutated = true;
        } else {
          i++;
        }
      }
    }

    if (!mutated) {
      return;
    }

    return arr.length === 1 ? arr[0] : arr;
  };
}

function extractFunctions(visitors, key) {
  let functions = [];
  for (let visitor of visitors) {
    let f = visitor[key];
    if (f) {
      functions.push(f);
    }
  }
  return functions;
}

function composeSimpleFunctions(res, visitors, key) {
  let functions = extractFunctions(visitors, key);
  if (functions.length === 0) {
    return;
  }

  if (functions.length === 1) {
    res[key] = functions[0];
    return;
  }

  res[key] = arg => {
    let mutated = false;
    for (let f of functions) {
      let res = f(arg);
      if (res) {
        arg = res;
        mutated = true;
      }
    }

    return mutated ? arg : undefined;
  };
}

function composeArrayFunctions(res, visitors, key) {
  let functions = extractFunctions(visitors, key);
  if (functions.length === 0) {
    return;
  }

  if (functions.length === 1) {
    res[key] = functions[0];
    return;
  }

  res[key] = composeResolvedArrayFunctions(functions);
}

function composeResolvedArrayFunctions(functions) {
  return arg => {
    let arr = [arg];
    let mutated = false;
    for (let f of functions) {
      for (let i = 0; i < arr.length;) {
        let item = arr[i];
        let res = f(item);
        if (Array.isArray(res)) {
          if (res.length === 0) {
            arr.splice(i, 1);
          } else if (res.length === 1) {
            arr[i++] = res[0];
          } else {
            arr.splice(i, 1, ...res);
            i += res.length;
          }
          mutated = true;
        } else if (res) {
          arr[i++] = res;
          mutated = true;
        } else {
          i++;
        }
      }
    }

    if (!mutated) {
      return;
    }

    return arr.length === 1 ? arr[0] : arr;
  };
}

class Bitset {
  constructor(maxBits = 32) {
    this.bits = 0;
    this.more = maxBits > 32 ? new Uint32Array(Math.ceil((maxBits - 32) / 32)) : null;
  }

  get(bit) {
    if (bit >= 32) {
      let i = Math.floor((bit - 32) / 32);
      let b = bit % 32;
      return Boolean(this.more[i] & (1 << b));
    } else {
      return Boolean(this.bits & (1 << bit));
    }
  }

  set(bit) {
    if (bit >= 32) {
      let i = Math.floor((bit - 32) / 32);
      let b = bit % 32;
      this.more[i] |= 1 << b;
    } else {
      this.bits |= 1 << bit;
    }
  }

  clear() {
    this.bits = 0;
    if (this.more) {
      this.more.fill(0);
    }
  }
}
