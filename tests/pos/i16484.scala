trait JWTEncoder:
  def encode[P](arg: String)(opt: Option[String] = None): String
  def encode[P](arg: String): String = encode(arg)()
