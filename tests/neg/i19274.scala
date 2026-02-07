import annotation.*

case class StaticBinding(v: String) {
  private def copy$default$1(): String = ??? // error
}

case class DynamicBinding(v: String):
  @targetName("copy$default$1")
  private def dynamo(): String = ??? // TODO

@main def Demo =
  val b = StaticBinding("test")
  println(b)
