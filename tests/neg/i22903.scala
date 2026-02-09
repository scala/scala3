class CustomProduct(x: Int) extends Product {
  def _1 = someName
  def _2 = blub

  val someName = x + 5
  val blub = "blub"

  override def canEqual(that: Any): Boolean = ???
}

object ProductMatch {
  def unapply(x: Int): CustomProduct = new CustomProduct(x)
}

@main
def run = {
  3 match {
    case ProductMatch(someName = x) => println (x) // error
  }
}