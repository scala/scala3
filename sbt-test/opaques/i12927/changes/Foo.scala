object Foo:
  opaque type BlaBla[+T, D] = Int
  extension [T, D](token: BlaBla[T, D]) def data: D = ???

//To cause the crash, after initial clean compilation
//replace `???` with `value.data` to cause the compiler crash
def foo[W <: Int](value: Bar.BlaBla[W]): Unit = value.data
