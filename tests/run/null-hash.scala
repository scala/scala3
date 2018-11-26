object Test {
  def f1 = List(5, 10, null: String|Null).##
  def f2(x: Any) = x.##
  def f3 = ((55, "abc", null: List[Int]|Null)).##

  def main(args: Array[String]): Unit = {
    f1
    f2(null)
    f2(null: String|Null)
    f3
    null.##
    (null: Any).##
    (null: String|Null).##
  }
}
