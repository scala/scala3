import java.io.IOException
import java.lang.NullPointerException
import java.lang.IllegalArgumentException

object IAE {
  def unapply(e: Exception): Option[String] =
    if (e.isInstanceOf[IllegalArgumentException]) Some(e.getMessage)
    else None
}

object EX extends Exception

trait ExceptionTrait extends Exception

object Test {
  def main(args: Array[String]): Unit = {
    var a: Int = 1
    try {
      throw new IllegalArgumentException()
    } catch {
      case e: IOException if e.getMessage == null =>
      case e: NullPointerException =>
      case e: IndexOutOfBoundsException =>
      case _: NoSuchElementException =>
      case _: ExceptionTrait =>
      case _: NoSuchElementException if a <= 1 =>
      case _: NullPointerException | _:IOException =>
      case `a` => // error: cannot compare
      case EX =>
      case IAE(msg) =>
      case e: IllegalArgumentException =>
    }
  }
}
