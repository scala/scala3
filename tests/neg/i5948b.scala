class Foo[T](var elem: T) { type TT = T }

object Test {
  def setFirstInPair[T](pair: (Foo[T], Foo[T])) = {
    pair._1.elem = pair._2.elem
  }

  def setFirstInList[T](list: List[Foo[T]]) = {
    list(0).elem = list(1).elem
  }

  def test1(): Unit = {
    val fooInt = new Foo(1)
    val fooString = new Foo("")
    val pair: (Foo[_], Foo[_]) = (fooInt, fooString)
    setFirstInPair(pair) // error
    println(fooInt.elem + 1)
  }

  def test2(): Unit = {
    val fooInt = new Foo(1)
    val fooString = new Foo("")
    val list: List[Foo[_]] = List(fooInt, fooString)
    setFirstInList(list) // error
    println(fooInt.elem + 1)
  }

  def main(args: Array[String]): Unit = {
    test1()
    test2()
  }
}