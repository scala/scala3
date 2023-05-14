object Test {
  def main(args: Array[String]): Unit = {
    val person = "Alice"
    println(s"\"Hello\", $person")
    println(s"""\"Hello\", $person""")
    println()
    println(f"\"Hello\", $person")
    println(f"""\"Hello\", $person""")
    println()
    println(raw"\"Hello\", $person")
    println(raw"""\"Hello\", $person""")
    println()
    println(s"\\TILT\\")
    println(f"\\TILT\\")
    println(raw"\\TILT\\")
    println()
    println(s"""\\TILT\\""")
    println(f"""\\TILT\\""")
    println(raw"""\\TILT\\""")
    println()
    println(raw"""\TILT\""")
  }
}
