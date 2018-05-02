object Flags {
  case class FlagSet(val bits: Long) {
    def toTermFlags = if (bits == 0) this else {
      println(size)                               // error
      FlagSet(bits & ~KINDFLAGS | TERMS)
    }
  }

  private val flagName = Array.fill(64, 2)("")

  private final val TERMindex = 0
  private final val TYPEindex = 1
  private final val TERMS = 1 << TERMindex
  private final val TYPES = 1 << TYPEindex
  private final val KINDFLAGS = TERMS | TYPES

  private def commonFlag(index: Int, name: String): FlagSet = {
    flagName(index)(TERMindex) = name
    flagName(index)(TYPEindex) = name
    FlagSet(TERMS | TYPES | (1L << index))
  }

  final val JavaStatic = commonFlag(31, "<static>")
  final val JavaStaticTerm = JavaStatic.toTermFlags  // error

  final val size: Int = ???
}
