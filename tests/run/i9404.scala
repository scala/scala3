import scala.reflect.Selectable.reflectiveSelectable

object Test {
  def main(args: Array[String]): Unit = {
    class Foo {
      def makeInt: Int = 5
      def testInt(x: Int): Unit = assert(5 == x)

      def makeRef: Option[String] = Some("hi")
      def testRef(x: Option[String]): Unit = assert(Some("hi") == x)
    }

    def test(foo: {
      def makeInt: Int
      def testInt(x: Int): Unit
      def makeRef: Option[String]
      def testRef(x: Option[String]): Unit
    }): Unit = {
      foo.testInt(foo.makeInt)
               //            ^
               // cannot infer type; expected type <?> is not fully defined
      foo.testRef(foo.makeRef)
               //            ^
               // cannot infer type; expected type <?> is not fully defined
    }

    test(new Foo)
  }
}