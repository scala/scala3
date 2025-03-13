package dotty.tools.pc.utils

/**
 * Helpers for generating variable names based on the desired types.
 */
object TermNameInference {

  /** Single character names for types. (`Int` => `i`, `i1`, `i2`, ...) */
  def singleLetterNameStream(typeName: String): LazyList[String] = {
    val typeName1 = sanitizeInput(typeName)
    val firstCharStr = typeName1.headOption.getOrElse('x').toLower.toString
    numberedStreamFromName(firstCharStr)
  }

  /** Names only from upper case letters (`OnDemandSymbolIndex` => `odsi`, `odsi1`, `odsi2`, ...) */
  def shortNameStream(typeName: String): LazyList[String] = {
    val typeName1 = sanitizeInput(typeName)
    val upperCases = typeName1.filter(_.isUpper).map(_.toLower)
    val name = if (upperCases.isEmpty) typeName1 else upperCases
    numberedStreamFromName(name)
  }

  /** Names from lower case letters (`OnDemandSymbolIndex` => `onDemandSymbolIndex`, `onDemandSymbolIndex1`, ...) */
  def fullNameStream(typeName: String): LazyList[String] = {
    val typeName1 = sanitizeInput(typeName)
    val withFirstLower =
      typeName1.headOption.map(_.toLower).getOrElse('x').toString + typeName1.drop(1)
    numberedStreamFromName(withFirstLower)
  }

  /** A lazy list of names: a, b, ..., z, aa, ab, ..., az, ba, bb, ... */
  def saneNamesStream: LazyList[String] = {
    val letters = ('a' to 'z').map(_.toString)
    def computeNext(acc: String): String = {
      if (acc.last == 'z')
        computeNext(acc.init) + letters.head
      else
        acc.init + letters(letters.indexOf(acc.last) + 1)
    }
    def loop(acc: String): LazyList[String] =
      acc #:: loop(computeNext(acc))
    loop("a")
  }

  private def sanitizeInput(typeName: String): String =
    typeName.filter(_.isLetterOrDigit)

  private def numberedStreamFromName(name: String): LazyList[String] = {
    val rest = LazyList.from(1).map(name + _)
    name #:: rest
  }
}
