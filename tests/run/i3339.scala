package outer {
  private class A() {
    override def toString = "A"
  }

  package inner {
    object Test {
      def main(args: Array[String]): Unit = {
        println(new A())  // now ok
      }
    }
  }
}

object Test extends App {
  outer.inner.Test.main(Array())
}