//> using options -Wunused:imports -Ystop-after:checkUnusedPostInlining

trait Schema[A]

case class Foo()
case class Bar()

trait SchemaGenerator[A] {
  given Schema[A] = new Schema[A]{}
}

object FooCodec extends SchemaGenerator[Foo]
object BarCodec extends SchemaGenerator[Bar]

def summonSchemas(using Schema[Foo], Schema[Bar]) = ()

def summonSchema(using Schema[Foo]) = ()

def `i19657 check prefix to pick selector`: Unit =
  import FooCodec.given
  import BarCodec.given
  summonSchemas

def `i19657 regression test`: Unit =
  import FooCodec.given
  import BarCodec.given // warn
  summonSchema

def `i19657 check prefix to pick specific selector`: Unit =
  import FooCodec.given_Schema_A
  import BarCodec.given_Schema_A
  summonSchemas

def `same symbol different names`: Unit =
  import FooCodec.given_Schema_A
  import FooCodec.given_Schema_A as AThing
  summonSchema(using given_Schema_A)
  summonSchema(using AThing)

package i17156:
  package a:
    trait Foo[A]
    object Foo:
      class Food[A] extends Foo[A]
      inline def derived[T]: Foo[T] = Food()

  package b:
    import a.Foo
    type Xd[A] = Foo[A]

  package c:
    import b.Xd
    trait Z derives Xd // checks if dealiased import is prefix a.Foo
    class Bar extends Xd[Int] // checks if import qual b is prefix of b.Xd

object Coll:
  class C:
    type HM[K, V] = scala.collection.mutable.HashMap[K, V]
object CC extends Coll.C
import CC.*

def `param type is imported`(map: HM[String, String]): Unit = println(map("hello, world"))

object Constants:
  final val i = 42
  def extra = 3
def `old-style constants are usages`: Unit =
  object Local:
    final val j = 27
  import Constants.i
  println(i + Local.j)

object Constantinople:
  val k = 42
class `scope of super`:
  import Constants.i // was bad warn
  class C(x: Int):
    def y = x
  class D(j: Int) extends C(i + j):
    import Constants.* // does not resolve i in C(i) and does not shadow named import
    def m = i // actually picks the higher-precedence import
    def f =
      import Constantinople.*
      class E(e: Int) extends C(i + k):
        def g = e + y + k + 1
      E(0).g
    def consume = extra // use the wildcard import from Constants

import scala.annotation.meta.*
object Alias {
  type A = Deprecated @param
}

// avoid reporting on runtime (nothing to do with transparent inline)
import scala.runtime.EnumValue

trait Lime

enum Color(val rgb: Int):
  case Red   extends Color(0xFF0000) with EnumValue
  case Green extends Color(0x00FF00) with Lime
  case Blue  extends Color(0x0000FF)

object prefixes:
  class C:
    object N:
      type U
  object Test:
    val c: C = ???
    def k2: c.N.U = ???
    import c.N.*
    def k3: U = ??? // TypeTree if not a select
  object Alt:
    val c: C = ???
    import c.N
    def k4: N.U = ???
end prefixes
