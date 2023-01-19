// scalac: -Yimports:scala,scala.Predef,hello.world.potions
//
class C {
  val v: Numb = magic // error // error
  def greet() = println("hello, world!")
}
// nopos-error
