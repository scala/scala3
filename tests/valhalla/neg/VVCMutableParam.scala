import scala.annotation.valhalla

@valhalla
class VVCMutableField(var a: Int) extends AnyVal // error

@valhalla
trait VTMutableField(var a: Int) extends Any // error