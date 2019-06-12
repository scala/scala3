object Logger {

  var indent = 0

  inline def log[T](msg: String)(op: => T): T = {
    println(s"${"  " * indent}start $msg")
    indent += 1
    val result = op
    indent -= 1
    println(s"${"  " * indent}$msg = $result")
    result
  }
}

object Logger2 {

  private var indent = 0
//   def inline$indent: Int = indent
//   def inline$indent_=(x$0: Int): Unit = indent = x$0

  inline def log[T](msg: String)(op: => T): T = {
    println(s"${"  " * indent}start $msg")
    indent += 1 // inline$indent = inline$indent + 1

    val result = op
    indent -= 1 // inline$indent = inline$indent - 1
    println(s"${"  " * indent}$msg = $result")
    result
  }
}
