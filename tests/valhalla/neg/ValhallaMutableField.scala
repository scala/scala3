import scala.annotation.valhalla

@valhalla
trait VTMutableField extends Any {
  var a = 5 // error
}

@valhalla
class VVCMutableField extends AnyVal {
  var a = 5 // error
}