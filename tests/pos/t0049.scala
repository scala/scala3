class C1(x: AnyRef) {};

class C2 extends C1({ class A extends AnyRef {}; (new A) : AnyRef }) {};

class Outer:
  class C2 extends C1({ class A extends AnyRef {}; (new A) : AnyRef }) {};
