import assert from 'assert';

import css from '../node/index.mjs';
import {browserslistToTargets} from '../node/index.mjs';

assert(typeof css === 'object');
assert(typeof browserslistToTargets === 'function');
