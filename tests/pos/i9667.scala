
def foo = new reflect.Selectable { object Foo }

trait A { type M }

val bar = new reflect.Selectable {
  class C extends A
  type B
  type D = C
  val x: C = ???
  val xx: x.M = ???
  val y: B = ???
  val z: C = ???
  val zz: z.M = ???
}
val bar1: reflect.Selectable{
  type B
  type D <: A
  val x: A
  val xx: Any
  val y: this.B
  val z: A
  val zz: Any
} = bar