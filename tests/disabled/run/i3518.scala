object Test {
  def main(args: Array[String]): Unit = {
    Unit.box(())
    Unit.unbox(Unit.box(()))
    println(Unit.box(()))
    println(Unit.unbox(Unit.box(())))
    println(Unit.box({
      println("A")
      ()
    }))
  }
}
