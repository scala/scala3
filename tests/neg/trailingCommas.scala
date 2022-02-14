package foo

// Multi-line only cases: make sure trailing commas are only supported when multi-line

trait ArgumentExprs1 { validMethod(23, "bar", )(Ev0, Ev1) } // error
trait ArgumentExprs2 { validMethod(23, "bar")(Ev0, Ev1, ) } // error
trait ArgumentExprs3 { new ValidClass(23, "bar", )(Ev0, Ev1) } // error
trait ArgumentExprs4 { new ValidClass(23, "bar")(Ev0, Ev1, ) } // error

trait Params1 { def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1 } // error // error

trait Params2 { def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1 } // error // error

trait ClassParams1 { final class C(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1) } // error
trait ClassParams2 { final class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1, ) } // error

trait SimpleExpr { (23, "bar", ) } // error
trait TypeArgs { def f: ValidGeneric[Int, String, ] } // error

trait TypeParamClause { type C[A, B, ] } // error
trait FunTypeParamClause { def f[A, B, ] } // error // error

trait SimpleType { def f: (Int, String, ) } // error
trait FunctionArgTypes { def f: (Int, String, ) => Boolean } // error

trait SimplePattern { val (foo, bar, ) = null: Any } // error

trait ImportSelectors { import foo.{ Ev0, Ev1, } } // error

trait Import { import foo.Ev0, foo.Ev1, } // error

trait ValDcl { val foo, bar, = 23 } // error

trait VarDef { var foo, bar, = _ } // error

trait PatDef { val Foo(foo), Bar(bar), = bippy } // error


// The Tuple 1 cases

// the Tuple1 value case: make sure that the possible "(23, )" syntax for Tuple1 doesn't compile to "23"
trait SimpleExpr2 { (23, ) } // error

// the Tuple1 type case: make sure that the possible "(Int, )" syntax for Tuple1[Int] doesn't compile to "Int"
trait SimpleType2 { def f: (Int, ) } // error

// Test utilities
object `package` {
  sealed trait Ev0; implicit object Ev0 extends Ev0
  sealed trait Ev1; implicit object Ev1 extends Ev1

  class ValidClass(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1)
  class ValidGeneric[A, B]
  def validMethod(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1): Int = 1

  case class Foo(foo: Any)
  case class Bar(foo: Any)
}
