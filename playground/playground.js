import init, {transform} from '../pkg';

let enc = new TextEncoder();
let inputs = document.querySelectorAll('input[type=number]');

async function update() {
  await init();

  let targets = {};
  for (let input of inputs) {
    if (input.value !== '') {
      targets[input.id] = input.valueAsNumber << 16;
    }
  }

  let res = transform({
    filename: 'test.css',
    code: enc.encode(source.value),
    minify: minify.checked,
    targets: Object.keys(targets).length === 0 ? null : targets
  });

  compiled.value = res;
}

update();
source.oninput = update;
for (let input of document.querySelectorAll('input')) {
  input.oninput = update;
}
