import scala.language.unsafeNulls

class SuperUniversalMethods:
  def a(): Class[?] = super.getClass()
  def b(): String = super.toString()
