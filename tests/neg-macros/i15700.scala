import scala.quoted.*

trait Foo:
  def xxx: Int

inline def foo(inline cond: Boolean) = ${ fooImpl('cond) }

def fooImpl(cond: Expr[Boolean])(using Quotes) =
  if cond.valueOrAbort then
    '{
      new Foo {
        override def xxx = 2
      }
    }
  else
    '{
      new Foo { // error: object creation impossible, since def xxx: Int in trait Foo is not defined
        override def xxxx = 1 // error: method xxxx overrides nothing
      }
    }
