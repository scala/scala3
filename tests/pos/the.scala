object Test {

  trait Foo { type T; val x: T }

  given intFoo: Foo {
    type T = Int
    val x = 3
  }

  val y: Int = summon[Foo].x

}