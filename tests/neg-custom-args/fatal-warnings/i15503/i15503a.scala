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
  import collection.mutable._ // error

  val bar = Set() // OK

object FooNestedUnused:
  import collection.mutable.Set // error
  object Nested:
    def hello = 1

object FooNested:
  import collection.mutable.Set // OK
  object Nested:
    def hello = Set()

