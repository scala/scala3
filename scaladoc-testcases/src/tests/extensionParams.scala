package tests.extensionParams

extension [A](thiz: A)
  def toTuple2[B](that: B): (A, B) 
  = thiz -> that
extension [A](a: A)(using Int)
  def f1[B](b: B): (A, B) 
  = ???

extension [A](a: A)(using Int)
  def f2(b: A): (A, A) 
  = ???

extension [A](a: A)(using Int)
  def f3(using String)(b: A): (A, A) 
  = ???

extension (a: Char)(using Int)
  def f4(using String)(b: Int): Unit 
  = ???

extension (a: Char)(using Int)
  def f5[B](using String)(b: B): Unit 
  = ???

extension [A <: List[Char]](a: A)(using Int)
  def f6[B](b: B): (A, B) 
  = ???

extension [A <: List[Char]](using String)(using Unit)(a: A)(using Int)(using Number)
  def f7[B, C](b: B)(c: C): (A, B) 
  = ???

extension [A <: List[Char]](using String)(using Unit)(a: A)(using Int)(using Number)
  def f8(b: Any)(c: Any): Any 
  = ???

extension [A <: List[Char]](using String)(using Unit)(a: A)(using Int)(using Number)
  def f9[B, C](using Int)(b: B)(c: C): (A, B) 
  = ???

extension [A <: List[Char]](using String)(using Unit)(a: A)(using Int)(using Number)
  def f10(using Int)(b: Any)(c: Any): Any 
  = ???
