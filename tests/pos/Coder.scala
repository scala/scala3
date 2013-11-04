import collection.mutable.HashMap

class Coder(words: List[String]) {

  (2 -> "ABC",  new ArrowAssoc('3') -> "DEF")

  private val mnemonics = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
      
  /** Invert the mnemonics map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
  private val charCode: Map[Char, Char] = mnemonics flatMap { ds =>
    val digit = ds._1
    val str = ds._2
    str map (ltr => ltr -> digit)
  }

//    for ((digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

    /** Maps a word to the digit string it can represent */
  private def wordCode(word: String): String = ???

  /** A map from digit strings to the words that represent them */
  private val wordsForNum: Map[String, List[String]] = ???

  /** All ways to encode a number as a list of words */
  def encode(number: String): Set[List[String]] = ???

  /** Maps a number to a list of all word phrases that can represent it */
  def translate(number: String): Set[String] = encode(number) map (_ mkString " ")

}
/*
/** Test code */
object Coder {
  def main(args : Array[String]) : Unit = {
    val coder = new Coder(List("Scala", "sobls", "Python", "Ruby", "C", "A", "rocks", "sucks", "works", "Racka"))
//    println(coder.wordsForNum)
    println(coder.translate("7225276257"))
  }
}*/
