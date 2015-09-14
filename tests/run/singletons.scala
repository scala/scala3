
object Test {

  val x: 1 = 1
  final val y = x
  val z: 1 = y

  object O { final val x = 42 }
  val fourtyTwo: 42 = O.x

  final val a = { println("a"); 2 } // side effect does not matter for type but needs to be preserved at runtime
  val b: 2 = a

  def f: 3 = 3
  final val c = f

  final def g: 4 = { println("g"); 4 } // side effect does not matter for type but needs to be preserved at runtime
  final val gv1: 4 = g
  final val gv2: 4 = g

  val dc: 3.0 = 3.0
  final val dc1 = dc
  val fc: 3.0f = 3.0f
  final val fc1 = fc

  val t: true = true

  val str: "" = ""
  final val str2 = str

  def main(args: Array[String]) = {
  }
}
/* To do: test that after erasure we have generated code like this:
 *
package <empty> {
  final lazy module val Test: Test$ = new Test$()
  final module class Test$() extends Object() { this: <notype> =>
    <accessor> def x(): Int = 1
    final <accessor> def y(): Int = 1
    <accessor> def z(): Int = 1
    final lazy module val O: Test.O$ = new Test.O$()
    final module class O$() extends Object() { this: <notype> =>
      final <accessor> def x(): Int = 42
    }
    <accessor> def fourtyTwo(): Int = 42
    final <accessor> def a(): Int = {
      println("a")
      2
    }
    <accessor> def b(): Int = 2
    def f(): Int = 3
    final <accessor> def c(): Int = Test.f()
    final def g(): Int = {
      println("g")
      4
    }
    final <accessor> def gv(): Int = Test.g()
    <accessor> def dc(): Double = 3.0
    final <accessor> def dc1(): Double = 3.0
    <accessor> def fc(): Float = 3.0
    final <accessor> def fc1(): Float = 3.0
    <accessor> def t(): Boolean = true
    <accessor> def str(): String = ""
    final <accessor> def str2(): String = ""
    def main(args: String[]): Unit = {
      ()
    }
  }
}
*/
