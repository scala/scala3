import scala.quoted._
def oops(using s: Scope) = {
  val q = '{ class Foo { val x = 3; ${ val v = 'x; '{} }  }}
}
