object Test{

  val xs: Seq[String] = List("Apple", "Orange", "Pear")
  export xs.*

}

val _ = Test.head
