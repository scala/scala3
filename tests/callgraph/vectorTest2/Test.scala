
package object foo {
  def bar(): Int = 42
}

package foo {
  object Foo {
    def main(args: Array[String]): Unit = {
      System.println(bar())
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    System.println(bar())
  }
}