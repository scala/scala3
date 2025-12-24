enum FormatPattern:
  case AsInt
  case AsLong

// some basic operations with enum:
def test =
  val p1 = FormatPattern.AsInt
  val p2 = FormatPattern.AsLong
  val p3 = FormatPattern.valueOf("AsInt")
  val p4 = FormatPattern.values(0)
  val ord1 = p1.ordinal
  val ord2 = p2.ordinal
  val str1 = p1.toString()
  val str2 = p2.toString()