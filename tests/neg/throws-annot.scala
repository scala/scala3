import java.io.IOException

@throws(classOf[IOException]) // error
class C

object O:
  @throws(classOf[IOException]) // error
  val x: Int = 0

class D:
  @throws(classOf[IOException]) // error
  type T = String

  def foo(@throws(classOf[IOException]) x: Int): Unit = () // error
