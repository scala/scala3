object Flags {
  private final val TERMindex = 0
  private final val TYPEindex = 1
  private final val TERMS = 1 << TERMindex
  private final val TYPES = 1 << TYPEindex

  case class FlagSet(val bits: Long) {
    def toTermFlags =
      if (bits == 0) this
      else FlagSet(bits & ~KINDFLAGS | TERMS)         // error: triggered from JavaStatic.toTermFlags
  }
  final val JavaStatic = FlagSet(31)
  final val JavaStaticTerm = JavaStatic.toTermFlags   // error: KINDFLAGS uninitialized

  private final val KINDFLAGS = identity(TERMS | TYPES)

  final val Private = FlagSet(2)
  final val PrivateTerm = Private.toTermFlags        //  ok
}