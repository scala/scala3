

object Test {

  val cls: java.lang.Class[_] = ???

  def myAsInstanceOf[T <: Class[T]](cls: java.lang.Class[_]): Class[T] = cls.asInstanceOf[Class[T]]
  Enum.valueOf(myAsInstanceOf(cls), "")




}
