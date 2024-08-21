package outer
package inner

sealed trait Foo
object Foo:
  trait TC[T]
  given ofFoo[T <: Foo]: TC[T] = ???
  trait Bar extends Foo

import Foo.TC
//Adding import Foo.Bar resolves the issue
val badSummon = summon[TC[Bar]]
  // was an ambiguous error, now OK, since the two references are the same

