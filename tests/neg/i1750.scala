trait Lang1 {
  trait Exp
  trait Visitor { def f(left: Exp): Unit }
  class Eval1 extends Visitor { self =>
    def f(left: Exp) = ()
  }
}
trait Lang2 extends Lang1 {
  class Visitor extends Eval1 { Visitor => // error: classes cannot be overridden
  }
}

