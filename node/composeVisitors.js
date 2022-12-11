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
  composeObjects(res, visitors, 'Rule', 'type');
  composeObjects(res, visitors, 'Property', 'property');
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
  composeTokenVisitors(res, 'Token', 'token', 'type', extractObjectsOrFunctions(visitors, 'Token'));
  composeTokenVisitors(res, 'Function', 'function', 'name', extractObjectsOrFunctions(visitors, 'Function'));
  composeTokenVisitors(res, 'Variable', 'var', null, [extractFunctions(visitors, 'Variable'), true]);
  // res.Token = createTokenVisitor(visitors, 'Token', 'type');
  // res.Function = createTokenVisitor(visitors, 'Function', 'name');
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

function composeObjects(res, visitors, key, typeKey) {
  let [values, hasFunction, allKeys] = extractObjectsOrFunctions(visitors, key);
  if (values.length === 0) {
    return;
  }

  if (values.length === 1) {
    res[key] = values[0];
    return;
  }

  if (hasFunction) {
    let functions = values.map(value => {
      if (typeof value === 'function') {
        return value;
      } else {
        return arg => {
          let f = value[arg[typeKey]];
          if (f) {
            return f(arg);
          }
        };
      }
    });

    res[key] = composeResolvedArrayFunctions(functions);
  } else {
    let v = {};
    for (let key of allKeys) {
      composeArrayFunctions(v, values, key);
    }
    res[key] = v;
  }
}

// function createTokenVisitor(visitors, type, typeKey) {
//   return arg => {
//     let arr = [{ type, value: arg }];
//     let mutated = false;
//     for (let visitor of visitors) {
//       for (let i = 0; i < arr.length;) {
//         let item = arr[i];
//         let f = visitor[item.type];
//         if (typeof f === 'object') {
//           f = f[item.value[typeKey]];
//         }
//         if (!f) {
//           i++;
//           continue;
//         }
//         let res = f(item.value);
//         if (Array.isArray(res)) {
//           if (res.length === 0) {
//             res.splice(i, 1);
//           } else if (res.length === 1) {
//             arr[i++] = res[0];
//           } else {
//             arr.splice(i, 1, ...res);
//             i += res.length;
//           }
//           mutated = true;
//         } else if (res) {
//           arr[i++] = res;
//           mutated = true;
//         } else {
//           i++;
//         }
//       }
//     }

//     if (!mutated) {
//       return;
//     }

//     return arr.length === 1 ? arr[0] : arr;
//   };
// }

function composeTokenVisitors(res, key, type, typeKey, [values, hasFunction, allKeys]) {
  // let [tokenValues, tokenHasFunction, tokenKeys] = extractObjectsOrFunctions(visitors, 'Token');
  // let [functionValues, functionHasFunction, functionKeys] = extractObjectsOrFunctions(visitors, 'Function');
  // let variableValues = extractFunctions(visitors, 'Variable');
  console.log(values)

  if (values.length === 0) {
    return;
  }

  if (values.length === 1) {
    res[key] = values[0];
    return;
  }

  let wrap = value => ({ type, value });
  if (hasFunction) {
    let functions = values.map(value => {
      if (typeof value === 'function') {
        return arg => {
          if (arg.type === type) {
            return value(arg.value);
          }
        };
      } else {
        return arg => {
          if (arg.type !== type) {
            return;
          }

          let f = value[arg.value[typeKey]];
          if (f) {
            return f(arg.value);
          }
        };
      }
    });

    res[key] = composeResolvedArrayFunctions(functions, wrap);
  } else {
    let v = {};
    for (let key of allKeys) {
      composeArrayFunctions(v, values, key, wrap);
    }
    res[key] = v;
  }
}

function composeSimpleFunctions(res, visitors, key) {
  let functions = [];
  for (let visitor of visitors) {
    let f = visitor[key];
    if (f) {
      functions.push(f);
    }
  }

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

function composeArrayFunctions(res, visitors, key, wrap) {
  let functions = extractFunctions(visitors, key);
  if (functions.length === 0) {
    return;
  }

  if (functions.length === 1) {
    res[key] = functions[0];
    return;
  }

  res[key] = composeResolvedArrayFunctions(functions, wrap);
}

function composeResolvedArrayFunctions(functions, wrap) {
  return arg => {
    let arr = [wrap ? wrap(arg) : arg];
    let mutated = false;
    for (let f of functions) {
      for (let i = 0; i < arr.length;) {
        let item = arr[i];
        let res = f(item);
        if (Array.isArray(res)) {
          if (res.length === 0) {
            res.splice(i, 1);
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
