import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';

let editor: monaco.editor.IStandaloneCodeEditor;
const output = document.getElementById('output')!;
let j8tw: typeof import('./j8tw').j8tw;

// Expected by wasm/src/lib.rs.
(window as any).now_ms = function() {
  return window.performance.now().toFixed(0);
};

function load() {
  fetch('js/0.bundle.js').then(r => r.text()).then(t => {
    editor.setValue(t);
    update();
  });
}

function update() {
  const js = editor.getValue();

  let start = Date.now();
  const out = j8tw(js);
  let end = Date.now();
  output.innerText = out + '\n\n' +
                     `${js.length} -> ${out.length} bytes (${(100*out.length/js.length).toFixed(1)}%)\n` +
                     `compilation took ${end - start}ms`;
}

async function main() {
  const wasm = await import("./j8tw");
  editor = monaco.editor.create(document.getElementById("input")!, {
    value: "function hello() {\n\talert('Hello world!');\n}",
    language: "javascript",
    lineNumbers: 'off',
    minimap: {enabled: false},
    scrollbar: {horizontalScrollbarSize: 5, verticalScrollbarSize: 5},
  });
  window.onresize = () => { editor.layout(); };

  j8tw = wasm.j8tw;
  document.getElementById('load')!.onclick = () => { load(); };
  editor.onDidChangeModelContent(event => { update(); });
  update();
}
main();
