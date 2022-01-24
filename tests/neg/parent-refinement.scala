
trait Id { type Value }
case class Year(value: Int) extends AnyVal
  with Id { type Value = Int }
  with Ordered[Year] { // error

}