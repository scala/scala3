import scala.collection.mutable.ListBuffer
class Foo {
  val zipped: ListBuffer[(String, Int)] = ???
  val unzipped:  (ListBuffer[String], ListBuffer[Int]) = zipped.unzip
}
