package collection

object Test {
  type AnyConstr[X] = Any
  val test: SeqOps[Char, AnyConstr, _] = null.asInstanceOf[StringOps]
}
