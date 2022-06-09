object ParserLocation {
  enum Location(val inParens: Boolean, val inPattern: Boolean, val inArgs: Boolean):
    case InParens      extends Location(true, false, false)
    case InArgs        extends Location(true, false, true)
    case InPattern     extends Location(false, true, false)
    case InGuard       extends Location(false, false, false)
    case InPatternArgs extends Location(false, true, true) // InParens not true, since it might be an alternative
    case InBlock       extends Location(false, false, false)
    case ElseWhere     extends Location(false, false, false)
}