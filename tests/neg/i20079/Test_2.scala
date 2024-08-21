object Test:
  val ints = List(1)
  Foo.xyz[Int, List](ints)
  Foo.xyz[Int, scala.collection.View](ints) // error
  Bar.xyz[Int, List](ints)
  Bar.xyz[Int, scala.collection.View](ints) // error