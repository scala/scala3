object Test {

  trait Foo { type T; val x: T }

  given Foo as intFoo {
    type T = Int
    val x = 3
  }

  val y: Int = summon[Foo].x

}