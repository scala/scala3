import scala.annotation.valhalla

@valhalla
trait VTMutableField extends Any {
  val a = 5 // error
}

@valhalla
class VVCMutableField extends AnyVal {
  val a = 5 // error
}