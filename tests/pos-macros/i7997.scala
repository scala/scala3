import scala.quoted._
def oops(using QuoteContext) = {
  val q = '{ class Foo { val x = 3; ${ val v = 'x; '{} }  }}
}
