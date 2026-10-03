object types:
  opaque type Struct = Int
  val test: Struct = 25
  extension (s: Struct)
    def field: Int = s
    def field_=(other: Int) = ()

@main def hello =
  import types.*
  test.field = "hello" // error