//> using options -Werror
val _ = locally{
  sealed abstract class PosInt(val value: Int) {
    override def equals(any: Any): Boolean = any.isInstanceOf[PosInt] // warn
  }
}
// nopos-error: No warnings can be incurred under -Werror.
