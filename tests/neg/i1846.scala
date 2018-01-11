object Test {
  def main(args: Array[String]): Unit = {
    val x = 42
    val Y = "h"

    x match { case { 42 } => 42 }                      // ok
    x match { case { 42.toString } => 42 }             // error
    x match { case { 42 }.toString => 42 }             // error
    x match { case { "h" }.toString => println(42) }   // error
    x match { case { "h".toString } => println(42) }   // error
    x match { case Y => println(42) }                  // error
    x match { case Y.toString => println(42) }         // error

    Y match { case Y.toString => println(42) }         // ok
    Y match { case { Y.toString } => println(42) }     // ok
    Y match { case { Y }.toString => println(42) }     // ok
  }
}
