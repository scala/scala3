import scala.scalajs.js
import scala.scalajs.js.annotation.*

enum MyEnum extends js.Object: // error
  case Foo

@js.native
@JSGlobal
enum MyEnumNative extends js.Object: // error
  case Bar

enum MyEnumAny extends js.Any: // error
  case Foo

@js.native
@JSGlobal
enum MyEnumNativeAny extends js.Any: // error
  case Bar
