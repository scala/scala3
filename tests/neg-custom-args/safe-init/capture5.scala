
object Names {
  trait PreName extends Any

  abstract class Name extends PreName

  /** Names for terms, can be simple or derived */
  abstract class TermName extends Name

  /** A simple name is essentially an interned string */
  final class SimpleName(val start: Int, val length: Int, private[Names] var next: SimpleName) extends TermName {
    def split: (TermName, SimpleName, String) = (EmptyTermName, this, "")
  }

  abstract class TypeName(val toTermName: TermName) extends Name

  // Nametable

  private final val InitialHashSize = 0x8000
  private final val InitialNameSize = 0x20000
  private final val fillFactor = 0.7

  /** Memory to store all names sequentially. */
   // because it's only mutated in synchronized block of termName
  var chrs: Array[Char] = new Array[Char](InitialNameSize)

  /** The number of characters filled. */
   // because it's only mutated in synchronized block of termName
  private[this] var nc = 0

  /** Hashtable for finding term names quickly. */
   // because it's only mutated in synchronized block of termName
  private[this] var table = new Array[SimpleName](InitialHashSize)

  /** The number of defined names. */
   // because it's only mutated in synchronized block of termName
  private[this] var size = 1

  table(0) = new SimpleName(-1, 0, null)    // error

  val EmptyTermName: TermName = table(0)
}
