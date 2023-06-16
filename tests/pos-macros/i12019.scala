trait A:
  type X >: Null

def ko1(f: (q: A) ?=> Int => q.X = null) = ()
def ko2(f: (q: A) ?=> Int => q.X = (_: A) ?=> null) = ()
def ko3(f: (q: A) => q.X = (q => null)) = ()


import scala.quoted.*

object Eg2 {

  // no default arg: ok
  def ok  (f: (q: Quotes) ?=> q.reflect.ValDef => q.reflect.Term) = ()

  // default the function *reference* to null: crash!
  def ko_1(f: (q: Quotes) ?=> q.reflect.ValDef => q.reflect.Term = null) = ()

  // default the function *result* to null: crash!
  def ko_2(f: (q: Quotes) ?=> q.reflect.ValDef => q.reflect.Term = (_: Quotes) ?=> null) = ()
}