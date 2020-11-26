package foo

trait Foo { def g(x: Any): Any }

inline given [T <: Foo] => T as f = ??? match {
  case x: T => x.g(10) // error
}

@main def Test = f
