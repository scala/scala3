trait T { self: B => }

abstract class A { self: B =>

}

class B extends A with T {
}

class C { self: B =>

}

class D extends A      // error

class E extends T      // error

object Test {

  new B() {}

  new A() {}   // error

  object O extends A  // error

  object M extends C // error

}
