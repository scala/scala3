// Scala Puzzler 54
object Test {
  case class Card(number: Int, suit: String = "clubs") {
    val value = (number % 13) + 1 // ace = 1, king = 13
    def isInDeck(implicit deck: List[Card]) = deck contains this
  }

  def main(args: Array[String]) = {
    implicit val deck = List(Card(1, "clubs"))
    implicit def intToCard(n: Int): Card = Card(n)
    assert(1.isInDeck)
  }
}
