
package foo

trait Foo { def g(x: Any): Any }

inline given f[T <: Foo]: T = ??? match {
  case x: T => x.g(10) // error // error
}

@main def Test = f
