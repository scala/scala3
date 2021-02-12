/** An iterator to return words in a string while keeping tarck of their offsets */
class WordSplitter(str: String, start: Int = 0, isSeparator: Char => Boolean = _ <= ' ')
extends Iterator[String]:
  private var idx: Int = start
  private var lastIdx: Int = start
  private var word: String = compiletime.uninitialized

  private def skipSeparators() =
    while idx < str.length && isSeparator(str(idx)) do
      idx += 1

  def lastWord = word
  def lastOffset = lastIdx

  def nextOffset =
    skipSeparators()
    idx

  def next(): String =
    skipSeparators()
    lastIdx = idx
    val b = new StringBuilder
    while idx < str.length && !isSeparator(str(idx)) do
      b += str(idx)
      idx += 1
    word = b.toString
    word

  def hasNext: Boolean = nextOffset < str.length
end WordSplitter