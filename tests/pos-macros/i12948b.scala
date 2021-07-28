import scala.quoted.*

package foo {
  trait Bar:
    def bar(using Quotes) = '{ Baz }

  private[foo] object Baz
}
