//> using options -Xfatal-warnings

object Test {
  def foo: Any = {
    open class Bar // warn
    new Bar
  }
}

// nopos-error: No warnings can be incurred under -Werror.