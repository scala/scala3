class A
object ObjA extends A
class B
object ObjB extends B

object Test {
  def foo[T <: A | B](t: T): List[T] = List(t)
  val x: ObjA.type | ObjB.type = ObjA

  // infers `T = ObjA$ | ObjB$` instead of `ObjA.type | ObjB.type` due to the
  // use of `widenSingleton` in TypeComparer#secondTry when the lhs is a union
  // type.
  val y = foo(ObjA : ObjA.type | ObjB.type)

  // This only compiles if `ObjA$ <:< ObjA.type`, there is a special-case in
  // `firstTry` for that but we also need a special case in `atoms` since unions
  // are involved.
  val z: List[ObjA.type | ObjB.type] = y
}
