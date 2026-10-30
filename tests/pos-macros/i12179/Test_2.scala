case class Wrap(value: String) extends AnyVal

def main(args: Array[String]): Unit = {
  val ctx = new EncoderContext()
  import ctx.*
  val w = new Wrap("stuff")
  SummonAndEncode[Wrap](w)
}
