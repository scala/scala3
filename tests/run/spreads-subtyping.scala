import language.experimental.multiSpreads

def foo(x: CharSequence*): String = x.mkString
val strings: Array[String] = Array("foo", "bar")

@main def Test =
  println(foo(("oof": CharSequence), strings*))