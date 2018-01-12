object Test {
  def main(args: Array[String]): Unit = {
    val x = 42
    val Y = "42"

    x match { case { 42 }           => () } // ok
    x match { case { "42".toInt }   => () } // ok
    x match { case { "42" }.toInt   => () } // ok
    x match { case { "42".toInt }   => () } // ok
    x match { case { Y.toInt }      => () } // ok
    x match { case { Y }.toInt      => () } // ok
    x match { case Y.toInt          => () } // ok
  }
}
