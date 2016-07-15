sealed class A
class B extends A
class C extends A

object Test {
  (null: A) match {
    case t: B =>
    case t: C =>
  }
}
