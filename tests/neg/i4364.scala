//> using options -Xfatal-warnings

object Test {
  def foo(c: java.util.function.Consumer[Integer]) = c.accept(0)
  def f(x: Int): Unit = ()

  def main(args: Array[String]) = {
    foo(f) // Ok: Consumer is @FunctionalInterface
    new java.io.ObjectOutputStream(f) // warn: OutputStream is not @FunctionalInterface
  }
}

// nopos-error: No warnings can be incurred under -Werror.