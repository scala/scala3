class Test {

  sealed trait Parent
  case class Foo(x: Int, y: Int, s: String) extends Parent
  case class Bar(x: Int, y: Int) extends Parent

  println(summon[deriving.Mirror.Of[Parent]])
}

object Test2 {

  case class Foo(x: Int, y: Int, s: String) extends i.Parent
  case class Bar(x: Int, y: Int) extends i.Parent

  val i = Inner()

  class Inner {

    sealed trait Parent

    println(summon[deriving.Mirror.Of[Parent]])
  }

}

object Test3 {
  val cw = new Test()
  val mirrorParent = summon[deriving.Mirror.Of[cw.Parent]]
}
