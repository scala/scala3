case class A(x: Option[Int], y: Option[Any])
case class B(x: Some[Int], y: Some[Boolean])

object Test extends App:
  import deriving.*

  val ma = summon[Mirror.ProductOf[A]]

  ma.fromProductTyped(B(Some(1), Some(true)))
  ma.fromProductTyped((Some(1), Some(false)))

