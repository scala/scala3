object Test {
  val sym0 = 's
  //sym0: Symbol
  sym0: 's // error

  //val sym1: 's = 's
  //sym1: Symbol
  //sym1: 's

  //final val sym2 = 's
  //sym2: Symbol
  //sym2: 's

  def id[T](t: T): T = t
  type Identity[T] = T
  def narrow[T <: Singleton](t: T): Identity[T] = t

  final val sym3 = id('s)
  //sym3: Symbol
  sym3: 's // error

  //val sym4 = narrow('s)
  //sym4: Symbol
  //sym4: 's
}
