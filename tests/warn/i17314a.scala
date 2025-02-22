//> using options -Werror -Wunused:all -deprecation -feature

package foo:
  class Foo[T]
  given Foo[Int] = new Foo[Int]


package bar:
  import foo.{given foo.Foo[Int]}
  import foo.Foo

  val repro: Foo[Int] = summon[Foo[Int]]
