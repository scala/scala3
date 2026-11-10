object Test:
  def main(args: Array[String]): Unit =
    val expected = args(0)
    assert(UsesMirror.docs == expected, s"UsesMirror: expected '$expected', got '${UsesMirror.docs}'")
    assert(UsesReflection.docs == expected, s"UsesReflection: expected '$expected', got '${UsesReflection.docs}'")
