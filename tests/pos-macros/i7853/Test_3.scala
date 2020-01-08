import SummonJsonEncoderTest._

case class PersonSimple(name:String, age:Int)

object Test {
  val stuff = PersonSimple("Joe", 123)

  def main(args: Array[String]):Unit = {
    println(SummonJsonEncoderTest.encodeAndMessAroundType(stuff) )
  }
}