package tests.extensionParams

extension [A](thiz: A)
  def toTuple2[B](that: B): (A, B) = thiz -> that

extension [A](a: A)(using Int)
  def f[B](b: B): (A, B) = ???

extension [A](a: A)(using Int)
  def ff(b: A): (A, A) = ???

extension [A](a: A)(using Int)
  def fff(using String)(b: A): (A, A) = ???

extension (a: Char)(using Int)
  def ffff(using String)(b: Int): Unit = ???

extension (a: Char)(using Int)
  def fffff[B](using String)(b: B): Unit = ???

extension [A <: List[Char]](a: A)(using Int)
  def ffffff[B](b: B): (A, B) = ???
