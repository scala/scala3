
trait A {
  class C {}
}

trait B extends A {
  class C {} // error: cannot override
}

trait C extends A {
  type C = Int // error: cannot override
}

