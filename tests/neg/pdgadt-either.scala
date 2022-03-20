trait T1
trait T2 extends T1
// T2 <:< T1

trait Expr[+X]
case class Tag1() extends Expr[T1]
case class Tag2() extends Expr[T2]

trait TypeTag { type A }

def foo(p: TypeTag, e: Expr[p.A]) = e match
  case _: (Tag2 | Tag1) =>
    // Tag1: (T2 <:) T1 <: p.A
    // Tag2: T2 <: p.A
    //   should choose T2 <: p.A
    val t1: p.A = ??? : T1  // error
    val t2: p.A = ??? : T2
