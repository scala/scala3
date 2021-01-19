
class T[A]

object T with
  implicit inline def derived[A]: T[A] = new T[A]
