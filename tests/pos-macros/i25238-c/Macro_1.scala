import scala.quoted.*

object Foo
object Holder { val v: String = "v" }

inline def objectRef(inline f: Int => Foo.type): Unit = ${ objectRefImpl('f) }
def objectRefImpl(f: Expr[Int => Foo.type])(using Quotes): Expr[Unit] =
  f match
    case '{ (x: Int) => Foo } => '{}
    case _ => quotes.reflect.report.errorAndAbort("No match")

inline def stableVal(inline f: Int => Holder.v.type): Unit = ${ stableValImpl('f) }
def stableValImpl(f: Expr[Int => Holder.v.type])(using Quotes): Expr[Unit] =
  f match
    case '{ ((x: Int) => Holder.v): (Int => Holder.v.type) } => '{}
    case _ => quotes.reflect.report.errorAndAbort("No match")

inline def dependent(inline f: (x: Int) => x.type): Unit = ${ dependentImpl('f) }
def dependentImpl(f: Expr[(x: Int) => x.type])(using Quotes): Expr[Unit] =
  f match
    case '{ ((x: Int) => x): ((x: Int) => x.type) } => '{}
    case _ => quotes.reflect.report.errorAndAbort("No match")
