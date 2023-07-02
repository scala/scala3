import language.`3.3`

def Test =

  val xo: Option[Int] = Some(1)

  val y =
    xo.fold:
      22
    .apply: x =>
      x + 1
  println(y)


  true // Should not count as the call true()
  ()

  // Setup
  def times(x: Int)(f: => Unit): Unit =
    (0 to x).foreach(_ => f)
  val credentials = Seq()
  object Path {def userHome = File}
  object File {def exists = true; def /(s: String) = this}
  def Credentials(f: File.type) = f
  val xs = List(1,2,3)
  def f(x: Int, g: Int => Int) = g(x)
  val x = 4

  // Copied from docs/_docs/reference/other-new-features/indentation.md

  times(10):
    println("ah")
    println("ha")

  credentials `++`:
    val file = Path.userHome / ".credentials"
    if file.exists
    then Seq(Credentials(file))
    else Seq()

  xs.map:
    x =>
      val y = x - 1
      y * y

  xs.foldLeft(0): (x, y) =>
    x + y

  {
    val x = 4
    f(x: Int, y =>
      x * (
        y + 1
      ) +
      (x +
      x)
    )
  }

  x match
  case 1 => print("I")
  case 2 => print("II")
  case 3 => print("III")
  case 4 => print("IV")
  case 5 => print("V")

  println(".")
