def test = {
  type M = { type T[+A] } & { type T[-A] }
  val M: M = ().asInstanceOf[M]
  M: M
}
