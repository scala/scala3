object Test {
  trait Transaction
  type Transactional[T] = (t: Transaction) ?=> T

  def ff(x: Int): Transactional[Int] = {
    summon[Transaction]
    x
  }

  def fff(x: Int): Transactional[Int] = {
    summon[Transaction]
    val x1 = ff(x)
    x1
  }

}