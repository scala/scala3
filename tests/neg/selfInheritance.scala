trait T { self: B => }

abstract class A { self: B =>

}

class B extends A with T {
}

class C { self: B =>

}

class D extends A // error: illegal inheritance: self type D of class D does not conform to self type B of parent class A

class E extends T // error: illegal inheritance: self type E of class E does not conform to self type B of parent trait T

object Test {

  new B() {}

  new A() {} // error: illegal inheritance: self type A{...} of anonymous class A{...} does not conform to self type B of parent class A

  object O extends A // error: illegal inheritance: self type Test.O.type of object O$ does not conform to self type B of parent class A

  object M extends C // error: illegal inheritance: self type Test.M.type of object M$ does not conform to self type B of parent class C

}

trait X { self: Y => } // error: missing requirement: self type Y & X of trait X does not conform to self type Z of required trait Y
trait Y { self: Z => }
trait Z
