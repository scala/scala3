object Test {
  def main(args: Array[String]): Unit = {
    val x = 42
    val Y = "42"

    x match { case { 42 }           => () } // error: pattern expected
    x match { case { 42.toString }  => () } // error: pattern expected
    x match { case { 42 }.toString  => () } // error: pattern expected
    x match { case "42".toInt       => () } // error: '=>' expected, but '.' found
    x match { case { "42".toInt }   => () } // error: pattern expected
    x match { case { "42" }.toInt   => () } // error: pattern expected
    x match { case { "42".toInt }   => () } // error: pattern expected
    x match { case Y                => () } // error: Values of types String and Int cannot be compared with == or !=
    x match { case { Y.toInt }      => () } // error: pattern expected
    x match { case { Y }.toInt      => () } // error: pattern expected
    x match { case { Y }.toString   => () } // error: pattern expected
    x match { case { Y.toString }   => () } // error: pattern expected
  }
}
