package test.conversions

object Conv {
  implicit val implicitLength: String => Int = _.length
}
