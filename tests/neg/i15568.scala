trait Foo[X >: Int <: String]

type Bar = Foo[? >: Int <: String] // error
