
object Test {

  val x: 1 = 1
  final val y = x
  val z: 1 = y

  object O { final val x = 42 }
  val fourtyTwo: 42 = O.x

  final val a = { println("x"); 2 } // side effects don't matter
  val b: 2 = a

  def f: 3 = 3
  val c = f

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
      println("x")
      2
    }
    <accessor> def b(): Int = 2
    def f(): Int = 3
    <accessor> def c(): Int = Test.f()
  }
}
*/
