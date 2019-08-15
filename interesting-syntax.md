
class C1 {
  static() {}
  static static() {}
}
new C1().static();
C1.static();


class C2 {
  0() {}
  "!"() {}
}
new C2()[0]();
new C2()['!']();


function f(...{length}) {
  return length;
}
f('a', 'b', 'c');  // 3


// Copy an array, ... kinda!
const x = {...['hello']};
