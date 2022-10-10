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

object FooGivenUnused:
  import SomeGivenImports.given // error

object FooGiven:
  import SomeGivenImports.given // OK
  import SomeGivenImports._ // error

  val foo = summon[Int]

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

/**
  * Some given values for the test
  */
object SomeGivenImports:
  given Int = 0
  given String = "foo"

