object Repro:
  val opt: Option[String] = Some("hi")
  val a: String = opt.orNull[String]
