
trait Txn[T]
trait Expr[X <: Txn[X], +Y]
trait BinOp[T, Repr[X <: Txn[X]] <: Expr[X, T]]

trait IntObj[T] extends Expr[T, Int]
trait IntBinOp extends BinOp[Int, IntObj]
object IntEq extends IntBinOp

object Test:
  val r: BinOp[?, ?] = IntEq : BinOp[Int, IntObj] // error: Required: BinOp[?, ?[X] <: Expr[X, BinOp[?, ?]#T]]
  // We would need the second wildcard to "depend" on the 1st one,
  // e.g. have SomeBinop[?] where `type SomeBinop[X] = BinOp[X, ? <: [Y] =>> Expr[Y, X]]`,
  // but this is an instance of an unreducible application of higher-kinded type to a wildcard argument.
  // Also note there would be no error if we made BinOp covariant in T.
