class Foo {
  def test1[A]: List[Int] | A => Int = {
    case ls: List[_] => ls.head       // error
    case _ => 0
  }

  def test2[A]: List[Int] | A => Int = {
    case ls: List[_] => ls.size
    case _ => 0
  }
}
