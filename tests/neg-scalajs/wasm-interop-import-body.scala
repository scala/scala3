import scala.scalajs.wasm
import scala.scalajs.wasm.annotation.*

object ImportBody {
  @WasmImport("env", "f")
  def imported(x: Int): Int = x // error

  @WasmImport("env", "g")
  def importedUnit(): Unit = () // error
}
