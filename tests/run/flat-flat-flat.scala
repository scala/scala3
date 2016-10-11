object Test {
  def f1 = List(Iterator(Some(1), None, Some(2)), Iterator(Some(3), None))
  def f2 = Iterator(List(Some(1), None, Some(2)), List(Some(3), None), Nil)
  def f3 = List(Some(Iterator(1)), None, Some(Iterator(2, 3)))
  def f4 = List(Some(Iterator(1)), Some(Iterator(2, 3)))
  def f5 = Iterator(List(Some(1), Some(2)), List(Some(3)), Nil)

  def main(args: Array[String]): Unit = {
    assert(f1.flatten.flatten.toList == List(1, 2, 3))
    assert(f5.flatten.flatten.toList == List(1, 2, 3))
    assert(f2.flatten.flatten.toList == List(1, 2, 3))
    assert(f3.flatten.flatten.toList == List(1, 2, 3))
    assert(f4.flatten.flatten.toList == List(1, 2, 3))
  }
}
