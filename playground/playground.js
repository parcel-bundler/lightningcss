import * as localWasm from '../node/pkg';

let wasm;
let enc = new TextEncoder();
let dec = new TextDecoder();
let inputs = document.querySelectorAll('input[type=number]');

async function loadVersions() {
  const {versions} = await fetch('https://data.jsdelivr.com/v1/package/npm/@parcel/css-wasm').then(r => r.json());
  versions
    .map(v => {
      const option = document.createElement('option');
      option.value = v;
      option.textContent = v;
      return option;
    })
    .forEach(o => {
      version.appendChild(o);
    })
}

async function loadWasm() {
  if (version.value === 'local') {
    wasm = localWasm;
  } else {
    wasm = await new Function('version', 'return import(`https://cdn.jsdelivr.net/npm/@parcel/css-wasm@${version}/parcel_css_node.js`)')(version.value);
  }
  await wasm.default();
}

function loadPlaygroundState() {
  const hash = window.location.hash.slice(1);
  try {
    const playgroundState = JSON.parse(decodeURIComponent(hash));
    reflectPlaygroundState(playgroundState);
  } catch {
    const initialPlaygroundState = {
      minify: minify.checked,
      nesting: nesting.checked,
      targets: getTargets(),
      source: source.value,
      version: version.value,
    };

    reflectPlaygroundState(initialPlaygroundState);
  }
}

function reflectPlaygroundState(playgroundState) {
  if (typeof playgroundState.minify !== 'undefined') {
    minify.checked = playgroundState.minify;
  }

  if (typeof playgroundState.cssModules !== 'undefined') {
    cssModules.checked = playgroundState.cssModules;
    compiledModules.hidden = !playgroundState.cssModules;
  }

  if (typeof playgroundState.analyzeDependencies !== 'undefined') {
    analyzeDependencies.checked = playgroundState.analyzeDependencies;
    compiledDependencies.hidden = !playgroundState.analyzeDependencies;
  }

  if (typeof playgroundState.nesting !== 'undefined') {
    nesting.checked = playgroundState.nesting;
  }

  if (typeof playgroundState.customMedia !== 'undefined') {
    customMedia.checked = playgroundState.customMedia;
  }

  if (playgroundState.targets) {
    const {targets} = playgroundState;
    for (let input of inputs) {
      let value = targets[input.id];
      input.value = value == null ? '' : value >> 16;
    }
  }

  if (playgroundState.source) {
    source.value = playgroundState.source;
  }

  if (playgroundState.unusedSymbols) {
    unusedSymbols.value = playgroundState.unusedSymbols.join('\n');
  }

  if (playgroundState.version) {
    version.value = playgroundState.version;
  }
}

function savePlaygroundState() {
  const playgroundState = {
    minify: minify.checked,
    nesting: nesting.checked,
    customMedia: customMedia.checked,
    cssModules: cssModules.checked,
    analyzeDependencies: analyzeDependencies.checked,
    targets: getTargets(),
    source: source.value,
    unusedSymbols: unusedSymbols.value.split('\n').map(v => v.trim()).filter(Boolean),
    version: version.value,
  };

  const hash = encodeURIComponent(JSON.stringify(playgroundState));

  if (
    typeof URL === 'function' &&
    typeof history === 'object' &&
    typeof history.replaceState === 'function'
  ) {
    const url = new URL(location.href);
    url.hash = hash;
    history.replaceState(null, null, url);
  } else {
    location.hash = hash;
  }
}

function getTargets() {
  let targets = {};
  for (let input of inputs) {
    if (input.value !== '') {
      targets[input.id] = input.valueAsNumber << 16;
    }
  }

  return targets;
}

function update() {
  const {transform} = wasm;

  const targets = getTargets();
  try {
    let res = transform({
      filename: 'test.css',
      code: enc.encode(source.value),
      minify: minify.checked,
      targets: Object.keys(targets).length === 0 ? null : targets,
      drafts: {
        nesting: nesting.checked,
        customMedia: customMedia.checked
      },
      cssModules: cssModules.checked,
      analyzeDependencies: analyzeDependencies.checked,
      unusedSymbols: unusedSymbols.value.split('\n').map(v => v.trim()).filter(Boolean)
    });

    compiled.value = dec.decode(res.code);
    compiled.style.color = "";
    compiledModules.value = JSON.stringify(res.exports, false, 2);
    compiledModules.hidden = !cssModules.checked;
    compiledDependencies.value = JSON.stringify(res.dependencies, false, 2);
    compiledDependencies.hidden = !analyzeDependencies.checked;
  } catch(e) {
    compiled.value = e.message;
    compiled.style.color = "red";
  }

  savePlaygroundState();
}

async function main() {
  loadPlaygroundState();
  await loadVersions();
  await loadWasm();

  update();
  source.oninput = update;
  unusedSymbols.oninput = update;
  for (let input of document.querySelectorAll('input')) {
    input.oninput = update;
  }
  version.onchange = async () => {
    await loadWasm();
    update();
  };
}

main();
