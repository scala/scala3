abstract class Foo {
  type A
  var elem: A
}

object Test {
  def setFirstInList[T](list: List[Foo { type A = T }]) = {
    list(0).elem = list(1).elem
  }

  def main(args: Array[String]): Unit = {
    val fooInt = new Foo { type A = Int; var elem = 1 }
    val fooString = new Foo { type A = String; var elem = "" }

    val list: List[Foo] = List(fooInt, fooString)
    setFirstInList[Foo#A](list) // error
    setFirstInList(list) // error
    println(fooInt.elem + 1)
  }
}