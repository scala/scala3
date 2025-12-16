extension (s: String)
  def f_::(using DummyImplicit): String = s.reverse
  def g_::(x: (suffix: String, n: Int)): String = s"$s${x.suffix * x.n}"
  def ok_::(using suffix: String, n: Int): String = s"$s${suffix * n}"
  def no_::(suffix: String, n: Int): String = s"$s${suffix * n}" // error
  def huh_::(using DummyImplicit)(suffix: String, n: Int): String = s"$s${suffix * n}"

def local =
  extension (s: String) def f_::(using DummyImplicit): String = s.reverse
  "hello, world".f_::

@main def Test =
  println:
    "hello, world".f_::
  println:
    f_::
      ("hello, world")
      (using DummyImplicit.dummyImplicit)
  println:
    (suffix = "s", n = 3).g_::("hello, world")
  println:
    "hello, world" g_:: ("s", 3)
  println:
    given String = "s"
    given Int = 3
    "hello, world".ok_::
  println:
    "hello, world".huh_::("s", 3)
