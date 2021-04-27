package scala.util.control {

object NonLocalReturns {

  class ReturnThrowable[T] extends ControlThrowable {
    private var myResult: T = compiletime.uninitialized
    def throwReturn(result: T): Nothing = {
      myResult = result
      throw this
    }
    def result: T = myResult
  }

  def throwReturn[T](result: T)(implicit returner: ReturnThrowable[T]): Nothing =
    returner.throwReturn(result)

  def returning[T](op: ReturnThrowable[T] ?=> T): T = {
    val returner = new ReturnThrowable[T]
    try op(using returner)
    catch {
      case ex: ReturnThrowable[_] =>
       if (ex `eq` returner) ex.result.asInstanceOf[T] else throw ex
    }
  }
}
}

object Test extends App {

  import scala.util.control.NonLocalReturns.*
  import scala.collection.mutable.ListBuffer

  def has(xs: List[Int], elem: Int) =
    returning {
      for (x <- xs)
        if (x == elem) throwReturn(true)
      false
    }

  def takeUntil(xs: List[Int], elem: Int) =
    returning {
      var buf = new ListBuffer[Int]
      for (x <- xs)
      yield {
        if (x == elem) throwReturn(buf.toList)
        buf += x
        x
      }
    }

  assert(has(List(1, 2, 3), 2))
  assert(takeUntil(List(1, 2, 3), 3) == List(1, 2))
}