object test:

  enum Option[+T]:
    case Some[T](x: T) extends Option[T]
    case None

  import Option._

  var x = Some(1)
  val y: Some[Int] = Some(2)
  var xc = y.copy(3)
  val yc: Some[Int] = y.copy(3)
  x = None
  xc = None


