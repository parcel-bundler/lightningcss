import init, {transform} from '../node/pkg/parcel_css_node.js';

let enc = new TextEncoder();
let dec = new TextDecoder();
let inputs = document.querySelectorAll('input[type=number]');

function loadPlaygroundState() {
  const hash = window.location.hash.slice(1);
  try {
    const playgroundState = JSON.parse(decodeURIComponent(hash));
    reflectPlaygroundState(playgroundState);
  } catch {
    const initialPlaygroundState = {
      minify: true,
      targets: {
        chrome: 95 << 16,
      },
      source: `.foo {
  background: yellow;
      
  -webkit-border-radius: 2px;
  -moz-border-radius: 2px;
  border-radius: 2px;
  
  -webkit-transition: background 200ms;
  -moz-transition: background 200ms;
  transition: background 200ms;
}`,
    };

    reflectPlaygroundState(initialPlaygroundState);
  }
}

function reflectPlaygroundState(playgroundState) {
  if (typeof playgroundState.minify !== 'undefined') {
    minify.checked = playgroundState.minify;
  }

  if (playgroundState.targets) {
    const {targets} = playgroundState;
    for (const target in targets) {
      const value = targets[target];
      if (value) {
        for (const input of Array.from(inputs)) {
          if (input.id === target) {
            input.value = value >> 16;
          }
        }
      }
    }
  }

  if (playgroundState.source) {
    source.value = playgroundState.source;
  }
}

function savePlaygroundState() {
  const playgroundState = {
    minify: minify.checked,
    targets: getTargets(),
    source: source.value,
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

async function update() {
  await init();

  const targets = getTargets();
  let res = transform({
    filename: 'test.css',
    code: enc.encode(source.value),
    minify: minify.checked,
    targets: Object.keys(targets).length === 0 ? null : targets,
  });

  compiled.value = dec.decode(res.code);

  savePlaygroundState();
}

loadPlaygroundState();

update();
source.oninput = update;
for (let input of document.querySelectorAll('input')) {
  input.oninput = update;
}
