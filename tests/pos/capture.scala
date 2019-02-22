// A condensation of shonan-hmm/Lifters.scala that shows why the approach
// of just skolemizing a tree as a hole to prepare it for capture conversion
// does not work.
// I tried two different stratgegies to adapt a tree whose widened type has wildcard
// arguments. Say the tree is `t` and the widened type is `C[_]`.
//
// skolemization-as-a-whole:
//
//    Convert `t` to  `t.cast[$i]` where `$i` is a skolem of type C[_]
//    This then relies on capture conversion for singleton types to do the rest.
//
// skolemization-of-each-param:
//
//    Convert `t` to `t.cast[C[$j.CAP]]` where `$j` is a skolem of type `TypeBox[Nothing, Any`]
//    (or more generally, `TypeBox[L, U]`) wgere `L` and `U` are the bounds of the wildcard).
//
// skolemization-of-each-param is more robust since it is stable under widening.
// By contrast, skolemization-as-a-whole risks losing capturing ability if the skolem
// type is widened in some context. This leads to the difference in behavior shown below.
class Test {

  abstract class Liftable[T]

  implicit def ClassIsLiftable[T]: Liftable[Class[T]] = new Liftable[Class[T]] {}

  class Expr[+T]

  implicit class LiftExprOps[T](val x: T) {
    def toExpr(implicit ev: Liftable[T]): Expr[T] = ???
  }

  def runtimeClass: Class[_] = ???

  runtimeClass.toExpr(ClassIsLiftable) // OK for skolemization-as-a-whole and skolemization-of-each-param

  runtimeClass.toExpr // only works with skolemization-of-each-param, also works in scalac.
}