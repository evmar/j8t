const input = document.getElementById('input')! as HTMLTextAreaElement;
const output = document.getElementById('output')!;
let j8tw: any;

// Expected by wasm/src/lib.rs.
(window as any).now_ms = function() {
  return window.performance.now().toFixed(0);
};

function load() {
  fetch('js/0.bundle.js').then(r => r.text()).then(t => {
    input.value = t;
    update();
  });
}

function update() {
  const js = input.value;
  let start = Date.now();
  const out = j8tw(js);
  let end = Date.now();
  output.innerText = out + '\n\n' +
                     `${js.length} -> ${out.length} bytes (${(100*out.length/js.length).toFixed(1)}%)\n` +
                     `compilation took ${end - start}ms`;
}

async function main() {
  const wasm = await import("./j8tw");
  j8tw = wasm.j8tw;
  document.getElementById('load')!.onclick = () => { load(); };
  input.oninput = () => { update(); };
  update();
}
main();
