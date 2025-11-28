//> using options -Werror -Wunused:imports

package util:
  class Random(s: String):
    def this() = this("")

package bar:
  export util.Random

package foo:
  import bar.Random
  def test = Random(42) // error

package baz:
  import bar.*
  def test = Random(42) // error
