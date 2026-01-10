@main def test() =
  var default: Option[String] = Some("test")
  var value: String = "0"

  extension (self: String)
            (using key: () => String = () => default.get)

    def int(`type`: String = "an integer"): Int =
      try
        self.toInt
      catch _ => throw new RuntimeException(`type`)

  def int(`type`: String = "an integer"): Int = value.int(`type`)
