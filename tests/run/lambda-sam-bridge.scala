class Meter(val x: Double) extends AnyVal

trait MySam[T] {
  def app(m: T): Any
}

object Test {
  val sam: MySam[Meter] = (x: Meter) => (x: Any)

  def main(args: Array[String]): Unit = {
    sam.app(new Meter(1.0))
  }
}


