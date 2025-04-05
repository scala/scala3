/* Compile with

    dotc implicits2.scala -Vprint:typer -Xprint-types -verbose

    and verify that the inserted wrapString comes from Predef. You should see

    val x: <root>.scala.collection.immutable.WrappedString =
      <
        <scala.Predef.wrapString:
          ((s: java.lang.String)scala.collection.immutable.WrappedString)
        >
      (<"abc":java.lang.String("abc")>):scala.collection.immutable.WrappedString
        >
*/
object implicits2 {

  val x: scala.collection.immutable.WrappedString = "abc"

  implicit val (xx: String, y: Int) = ("a", 22)

  def main(args: Array[String]) = {
    println(implicitly[String])
    println(implicitly[Int])
  }
}
