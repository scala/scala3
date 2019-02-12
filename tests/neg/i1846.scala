object Test {
  def main(args: Array[String]): Unit = {
    val x = 42
    val Y = "42"

    x match { case { 42 }           => () } // error // error
    x match { case { 42.toString }  => () } // error // error
    x match { case { 42 }.toString  => () } // error // error
    x match { case "42".toInt       => () } // error
    x match { case { "42".toInt }   => () } // error // error
    x match { case { "42" }.toInt   => () } // error // error
    x match { case { "42".toInt }   => () } // error // error
    x match { case Y                => () } // error
    x match { case { Y.toInt }      => () } // error // error
    x match { case { Y }.toInt      => () } // error // error
    x match { case { Y }.toString   => () } // error // error
    x match { case { Y.toString }   => () } // error // error
  }
}
