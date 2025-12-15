extension (s: String)
  def f_::(using DummyImplicit): String = s.reverse // error
  def g_::(x: (suffix: String, n: Int)): String = s"$s${x.suffix * x.n}"
  def ok_::(using suffix: String, n: Int): String = s"$s${suffix * n}" // error
  def no_::(suffix: String, n: Int): String = s"$s${suffix * n}" // error

@main def Test =
  println:
    "hello, world".f_::
  println:
    (suffix = "s", n = 3).g_::("hello, world")
  println:
    "hello, world" g_:: ("s", 3)
  println:
    given String = "s"
    given Int = 3
    "hello, world".ok_::
