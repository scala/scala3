package tests.extensionParams

trait Animal

extension [A](thiz: A)
  def toTuple2[B](that: B): (A, B)
   = thiz -> that

extension [A](a: A)(using Int)
  def f1[B](b: B): (A, B)
  = ???

extension [A](a: A)(using String)
  def f2(b: A): (A, A)
  = ???

extension [A](a: A)(using Number)
  def f3(using String)(b: A): (A, A)
  = ???

extension (a: Char)(using Int)
  def f4(using String)(b: Int): Unit
  = ???

extension (a: String)(using Int)
  def f5[B](using String)(b: B): Unit
  = ???

extension [A <: List[Char]](a: Int)(using Int)
  def f6[B](b: B): (A, B)
  = ???

extension [A <: List[Char]](using String)(using Unit)(a: A)(using Int)(using Number)
  def f7[B, C](b: B)(c: C): (A, B)
  = ???

extension [A <: List[Char]](using String)(using Number)(a: A)(using Int)(using Unit)
  def f8(b: Any)(c: Any): Any
  = ???

extension [A <: List[Char]](using Unit)(using String)(a: A)(using Int)(using Number)
  def f9[B, C](using Int)(b: B)(c: C): (A, B)
  = ???

extension [A <: List[Char]](using Number)(using Unit)(a: A)(using Int)(using String)
  def f10(using Int)(b: Any)(c: Any): Any
   = ???

  def f12(using Int)(b: A)(c: String): Number
   = ???

extension (using String)(using Unit)(a: Animal)(using Int)(using Number)
  def f11(b: Any)(c: Any): Any
  = ???

extension (a: Int)
  def f13(): Any
  = ???

extension (using Unit)(a: Int)
  def f14(): Any
  = ???

extension (using String)(using Int)(a: Animal)(using Unit)(using Number)
  def f16(b: Any)[T](c: T): T
  = ???
  def f17[D](b: D)[T](c: T): T
  = ???



