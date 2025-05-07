//> using options -Wunused:all

package foo:
  class Foo[T]
  given Foo[Int] = new Foo[Int]


package bar:
  import foo.{given foo.Foo[Int]} // warn
  import foo.Foo

  given Foo[Int] = ???

  val repro: Foo[Int] = summon[Foo[Int]]

