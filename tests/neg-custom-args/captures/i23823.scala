trait IO:
  def f(): Unit

case class BoxIO[+T <: IO^](value: T) extends IO:
  def this(value: T, x: Int) = this(value)
  def f(): Unit = value.f()

def test(consume io: IO^): IO =
  val b: BoxIO[IO^] = BoxIO(io) // error
  val b2: BoxIO[IO^] = new BoxIO(io) // error
  val a: IO = b // leak
  a