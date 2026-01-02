//> using options -Wunused:imports

package util:
  class Random(s: String):
    def this() = this("")

package bar:
  export util.Random

package foo:
  import bar.Random
  def test = Random(null)

package baz:
  import bar.*
  def test = Random(null)

// cf tests/pos/i12299a.scala
package aliased:
  object Outer:

    object Wrap:
      export Outer.Bar

    class Bar

    def test =
      import Wrap.* // warn! not a naming error because a benign alias
      val localBar = Bar() // not Wrap.Bar!
      val localNewBar = new Bar

package i24879:
  class C:
    class D

  class Baz:
    val c = new C
    export c.*

  def test =
    val baz = Baz()
    baz.D() // no warn and no crash
