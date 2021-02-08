import Predef.{byte2Byte as _, *}
import scala.concurrent.Future

val f = Future[Unit] { } // error

val b: java.lang.Byte = (1: Byte) // error, but no hint
