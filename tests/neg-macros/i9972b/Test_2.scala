
class T[A]

object T:
  implicit inline def derived[A]: T[A] = new T[A]
