//> using options -Xfatal-warnings

class A
class B
class C

object Test {
  def foo(x: A) = x match {
    case x: B => x // error: this case is unreachable since class A is not a subclass of class B
    case _ =>
  }

  def bar(x: A | B) = x match {
    case x: C => x // error
    case _ =>
  }

  def baz(x: A) = x match {
    case x: (B | C) => x // warn
    case _          =>
  }
}

// nopos-error: No warnings can be incurred under -Werror.