//> using options -Xfatal-warnings

package foo

trait Foo { def g(x: Int): Any }

object Test:

  inline given f[T <: Foo]: T = ??? match {
    case x: T => x.g(10) // error
  }

  @main def Test = f
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
