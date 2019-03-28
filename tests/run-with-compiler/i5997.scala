object Test {
  def main(args: Array[String]): Unit = {
    import quoted.Toolbox.Default._
    val v = '{ (if true then Some(1) else None).map(v => v+1) }
    println(v.show)
  }
}
