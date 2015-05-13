abstract class  A {
  override def toString(): String // crucial

  def toString(sb: StringBuilder): StringBuilder // crucial
}

case class B() extends A {
  // overloaded version is implemented, causing toString not to be implemented?
  def toString(sb: StringBuilder): StringBuilder = sys.error("")
}

object Test extends dotty.runtime.LegacyApp {
  Console.println(B)
}
