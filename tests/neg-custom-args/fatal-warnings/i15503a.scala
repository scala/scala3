// scalac: -Wunused:imports


object FooUnused:
  import collection.mutable.Set // error
  import collection.mutable.{Map => MutMap} // error
  import collection.mutable._ // error

object FooWildcardUnused:
  import collection.mutable._ // error

object Foo:
  import collection.mutable.Set // OK
  import collection.mutable.{Map => MutMap} // OK

  val bar = Set() // OK
  val baz = MutMap() // OK

object FooWildcard:
  import collection.mutable._ // OK

  val bar = Set() // OK

object FooNestedUnused:
  import collection.mutable.Set // error
  object Nested:
    def hello = 1

object FooNested:
  import collection.mutable.Set // OK
  object Nested:
    def hello = Set()


/**
 * Import used as type name are considered
 * as used.
 *
 * Import here are only used as types, not as
 * Term
 */
object FooTypeName:
  import collection.mutable.Set // OK
  import collection.mutable.Map // OK
  import collection.mutable.Seq // OK
  import collection.mutable.ArrayBuilder // OK
  import collection.mutable.ListBuffer // error

  def checkImplicit[A](using Set[A]) = ()
  def checkParamType[B](a: Map[B,B]): Seq[B] = ???
  def checkTypeParam[A] = ()

  checkTypeParam[ArrayBuilder[Int]]


object InlineChecks:
  object InlineFoo:
    import collection.mutable.Set // OK
    import collection.mutable.Map // error
    inline def getSet = Set(1)

  object InlinedBar:
    import collection.mutable.Set // error
    import collection.mutable.Map // error
    val a = InlineFoo.getSet

object MacroChecks:
  object StringInterpol:
    import collection.mutable.Set // OK
    import collection.mutable.Map // OK
    println(s"This is a mutableSet : ${Set[Map[Int,Int]]()}")


object InnerMostCheck:
  import collection.mutable.* // error
  def check =
    import collection.mutable.* //OK
    val a = Set(1)

object IgnoreExclusion:
  import collection.mutable.{Set => _} // OK
  import collection.mutable.{Map => _} // OK
  import collection.mutable.{ListBuffer} // error
  def check =
    val a = Set(1)
    val b = Map(1 -> 2)

/* BEGIN : Check on packages*/
package p {
  class C
}

package p {
  import p._ // error
  package q {
    class U {
      def f = new C
    }
  }
}
/* END : Check on packages*/

/* BEGIN : tests on meta-language features */
object TestLanguageImportAreIgnored:
  /* note: scala3 Conversion[U,T] do not require an import */
  import language.implicitConversions // OK
  import language._ // OK

  implicit def doubleToInt(d:Double):Int = d.toInt

  def idInt(i:Int):Int = i
  val someInt = idInt(1.0)

object TestTailrecImport:
  import annotation.tailrec // OK
  @tailrec
  def fac(x:Int, acc:Int = 1): Int =
    if x == 0 then acc else fac(x - 1, acc * x)
/* END : tests on meta-language features */

/* BEGIN : tests on given import order */
object GivenImportOrderAtoB:
  class X
  class Y extends X
  object A { implicit val x: X = new X }
  object B { implicit val y: Y = new Y }
  class C {
    import A._ // error
    import B._
    def t = implicitly[X]
  }

object GivenImportOrderBtoA:
  class X
  class Y extends X
  object A { implicit val x: X = new X }
  object B { implicit val y: Y = new Y }
  class C {
    import B._
    import A._ // error
    def t = implicitly[X]
  }
/* END : tests on given import order */

/*
 * Advanced tests on given imports meta-programming
 *
 * - Currently also tests that no imported implicits are reported
 */

package summoninlineconflict:
  package lib:
    trait A
    trait B
    trait C
    trait X

    given willBeUnused: (A & X) = new A with X {}
    given willBeUsed: (A & B) = new A with B {}
    given notUsedAtAll: Int = 0

  package use:
    import lib.{A, B, C, willBeUnused, willBeUsed, notUsedAtAll} // OK
    import compiletime.summonInline // OK

    transparent inline given conflictInside: C =
      summonInline[A]
      new {}

    transparent inline given potentialConflict: C =
      summonInline[B]
      new {}

    val b: B = summon[B]
    val c: C = summon[C]

package unusedgivensimports:
  package foo:
    given Int = 0

  package bar:
    import foo.given // OK