trait Lang1 {
  trait Exp
  trait Visitor { def f(left: Exp): Unit }
  class Eval1 extends Visitor { self =>
    def f(left: Exp) = ()
  }
}
trait Lang3 { self: Lang1 =>
  class Visitor extends Eval1 { Visitor => // error: classes cannot be overridden
  }
}
trait Lang4 extends Lang1 with Lang3

