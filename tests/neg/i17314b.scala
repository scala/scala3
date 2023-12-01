// scalac: -Xfatal-warnings -Wunused:all

package foo:
  class Foo[T]
  given Foo[Int] = new Foo[Int]


package bar:
  import foo.{given foo.Foo[Int]} // error
  import foo.Foo

  given Foo[Int] = ???

  val repro: Foo[Int] = summon[Foo[Int]]
