import kc.Encoder

def test: Unit =
  val cellEncoder = new Encoder[String] {
    override def encode: String = ""
  }
