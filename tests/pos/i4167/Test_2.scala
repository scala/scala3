package collection

object Test {
  type AnyConstr[X] = Any
  val test: SeqOps[Char, AnyConstr, ?] = null.asInstanceOf[StringOps]
}
