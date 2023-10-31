

object Test {

  val cls: java.lang.Class[?] = ???

  def myAsInstanceOf[T <: Class[T]](cls: java.lang.Class[?]): Class[T] = cls.asInstanceOf[Class[T]]
  Enum.valueOf(myAsInstanceOf(cls), "")




}
