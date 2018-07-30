import * as monaco from 'monaco-editor/esm/vs/editor/editor.api';

let editor: monaco.editor.IStandaloneCodeEditor;
const output = document.getElementById('output')!;
let j8tw: typeof import('./j8tw');

// Matches wasm/src/lib.rs types.
interface Pos {
    line: number;
    col: number;
}
interface Result {
    output: string;
    error: {
        msg: string;
        start: Pos;
        end: Pos;
    };
}

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
  const res: Result = j8tw.run(js);
  let end = Date.now();

  let markers: monaco.editor.IMarkerData[] = [];
  if (res.error.msg) {
    markers.push({
      message: res.error.msg,
      severity: monaco.MarkerSeverity.Error,
      startLineNumber: res.error.start.line,
      startColumn: res.error.start.col,
      endLineNumber: res.error.end.line,
      endColumn: res.error.end.col,
    });
  }
  monaco.editor.setModelMarkers(editor.getModel(), 'j8t', markers);

  const out = res.output;
  output.innerText = out + '\n\n' +
                     `${js.length} -> ${out.length} bytes (${(100*out.length/js.length).toFixed(1)}%)\n` +
                     `compilation took ${end - start}ms`;
}

async function main() {
  j8tw = await import("./j8tw");
  editor = monaco.editor.create(document.getElementById("input")!, {
    value: "function hello() {\n\talert('Hello world!');\n}",
    language: "javascript",
    lineNumbers: 'off',
    minimap: {enabled: false},
    scrollbar: {horizontalScrollbarSize: 5, verticalScrollbarSize: 5},
  });
  window.onresize = () => { editor.layout(); };

  document.getElementById('load')!.onclick = () => { load(); };
  editor.onDidChangeModelContent(event => { update(); });
  update();
}
main();
