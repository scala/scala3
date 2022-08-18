// scalajs: --skip

class MT(sf: MT => String) {
  // `s` is retained as a field, but `sf` should not be.
  val s = sf(this)
}
object Test extends App {
  def printFields(obj: Any) =
    println(obj.getClass.getDeclaredFields.map(_.toString).sorted.toList.mkString("\n"))
  printFields(new MT(_ => ""))
}
