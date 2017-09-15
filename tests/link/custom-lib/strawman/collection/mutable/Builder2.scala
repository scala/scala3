package strawman.collection.mutable

trait Builder2[To] {
  def result(): To
}

class StringBuilder2 extends Builder2[String] {
  def result() = "hello"
}
