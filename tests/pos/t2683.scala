class A
class B extends A

object Test {
  val c: Class[? <: A] = Class.forName("B").asSubclass(classOf[A])
  val x: Option[Class[? <: A]] = Some(3).map { case _ => c }
}
