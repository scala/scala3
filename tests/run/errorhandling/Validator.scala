package scala.util
import boundary.{break, Label}
import collection.mutable

object Validator {
  def validate[E](op: Validator[E] => Unit): Validator[E] =
    boundary: lbl ?=>
      val v = Validator[E]()
      op(v)
      v
}

class Validator[E] private()(using lbl: Label[Validator[E]]) {
  private val errors = mutable.ListBuffer[E]()

  def ensure(p: Boolean, e: => E, abort: Boolean = false): Unit =
    if !p then
      errors += e
      if abort then break(this)

  def ifOK[T](t: => T): Result[T, List[E]] =
    if errors.isEmpty then Ok(t) else Err(errors.toList)

}

