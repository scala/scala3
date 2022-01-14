object ProdNonEmpty {
  def _1: Int = 0
  def _2: String = "???" // Slight variation with nsc: this test passes
                         // with ??? here. I think dotty behavior is fine
                         // according to the spec given that methods involved
                         // in pattern matching should be pure.
  def isEmpty = false
  def unapply(s: String): this.type = this
  def get = this
}

object ProdEmpty {
  def _1: Int = ???
  def _2: String = ???
  def isEmpty = true
  def unapply(s: String): this.type = this
  def get = this
}

object Test {
  def main(args: Array[String]): Unit = {
    "" match {
      case ProdNonEmpty(0, _) => ()
      case _ => ???
    }

    "" match {
      case ProdNonEmpty(1, _) => ???
      case _ => ()
    }

    "" match {
      case ProdEmpty(_, _) => ???
      case _ => ()
    }
  }
}
