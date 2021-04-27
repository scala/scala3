object Bug {
  abstract class FChain {
    type T

    def chain(constant:String) =
      new FConstant[this.type](constant, this) //removing [this.type], everything compiles
  }

  case class FConstant[E <: FChain](constant:String, tail:E) extends FChain {
    type T = tail.T
  }

  object FNil extends FChain {
    type T = Unit
  }

}

object Test {
  import Bug.*
  println("Compiles:")
  val a1 = FNil.chain("a").chain("a")
  val a2 = a1.chain("a")

  println("\nDoesn't compile:")
  val a3 = FNil.chain("a").chain("a").chain("a")
  val a4: FConstant[_ <: FConstant[_ <: FConstant[FNil.type]]] = a3
}
