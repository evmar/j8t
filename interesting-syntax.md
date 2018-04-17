
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
