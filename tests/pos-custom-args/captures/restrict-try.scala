import caps.{SharedCapability, Control, Unscoped}

class Try[+T]
case class Ok[T](x: T) extends Try[T]
case class Fail(ex: Exception) extends Try[Nothing]

trait Matrix extends Unscoped:
  def get(): Unit

trait Label extends Control:
  def break(): Unit

object Try:
  def apply[T](body: () => T): Try[T]^{body.only[Control]} =
    try Ok(body())
    catch case ex: Exception => Fail(ex)

def Test(m: Matrix^, l: Label) =
  val x =
    Try:
      val b = () =>
        m.get()
        l.break()
      val _: () ->{m, l} Unit = b
      b
  val y: Try[Unit]^{l} = x



