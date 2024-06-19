package example

object Main {
  def main(a: Array[String]): Unit = {
    val p: PolyFunction = // error: PolyFunction subtypes must refine the apply method
      [A] => (xs: List[A]) => xs.headOption
  }
}
