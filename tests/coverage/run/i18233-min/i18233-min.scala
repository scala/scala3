def aList =
  List(Array[String]()*)

def arr =
  Array("abc", "def")

def anotherList =
  List(arr*)

object Test extends App {
  println(aList)
  println(anotherList)
}
