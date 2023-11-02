//> using options -Xfatal-warnings

object foo {
  @main def foo(x: Int) = () // warn: class foo and object foo produce classes that overwrite one another
}

// nopos-error: No warnings can be incurred under -Werror.