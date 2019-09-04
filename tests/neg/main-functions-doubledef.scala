
class bar
object bar {
  @main def bar(x: Int) = () // error: class bar has already been compiled once during this run
}

object baz {
  @main def bam(x: Int): Unit = ()
  @main def bam(x: String): Unit = () // error: class bam has already been compiled once during this run
}
