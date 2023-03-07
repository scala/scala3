package testcode

class A

class B

object ExtensionA {
  extension (self: A) {
    def id = "A"
  }
}
object ExtensionB {
  extension (self: B) {
    def id = "B"
  }
}

object Main {
  def main1(args: Array[String]): Unit = {
    import ExtensionB._
    import ExtensionA._
    val a = A()
    println(a.id) // error
  }
  def main2(args: Array[String]): Unit = {
    import ExtensionA._
    import ExtensionB._
    val a = A()
    println(a.id) // error
  }
}