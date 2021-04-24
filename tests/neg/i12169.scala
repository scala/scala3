object Var:
  class Expanded[T <: Txn[T], B] extends Form[T]

trait Txn[T <: Txn[T]]

trait Form[T]

class TT extends Txn[TT]

private final class FlatVarCellView[T <: Txn[T], B](
  firstVr  : Option[Var.Expanded[TT, B]]
)

def Test =
  val opt: Option[Form[TT]] = ???
  val firstVr = opt match
    case Some(ex: Var.Expanded[TT, _]) => Some(ex)
    case _                            => None
  new FlatVarCellView(firstVr)  // error
   //                      Found:    (firstVr : Option[Var.Expanded[TT, ?]])
   //                      Required: Option[Var.Expanded[TT, B]]
   //
   //                      where:    B is a type variable
   //
   // Note that we cannot do capture conversion since the `?` does not appear as an argument
   // of the a type. It's dubious whether capture conversion for more deeply nested types
   // would be sound.

  // Remedy:
  opt match
    case Some(ex: Var.Expanded[TT, _]) => new FlatVarCellView(Some(ex))
      // here, we instantiate `B` with the unnamed second parameter of `Var.Expanded`
    case _                             => new FlatVarCellView(None)
  opt match
    case Some(ex: Var.Expanded[TT, t]) => new FlatVarCellView[TT, t](Some(ex))
      // the same as above, spelt out
    case _                             => new FlatVarCellView(None)
