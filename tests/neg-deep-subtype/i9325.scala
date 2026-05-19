enum Foo[T] extends java.lang.Enum[Foo[T]] { case Red extends Foo[Int]; case Blue extends Foo[String] } // error: An enum extending java.lang.Enum cannot have type parameters
val res0 = (Foo.Red: Foo[?]) compareTo Foo.Blue // error: type mismatch Found (Foo.Blue : Foo[String]) Expected ?1.E
