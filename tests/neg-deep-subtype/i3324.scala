//> using options -Xfatal-warnings

class Foo {
  def foo(x: Any): Boolean =
    x.isInstanceOf[List[String]]  // warn
}

// nopos-error: No warnings can be incurred under -Werror.