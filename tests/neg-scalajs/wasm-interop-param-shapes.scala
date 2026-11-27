import scala.scalajs.wasm
import scala.scalajs.wasm.annotation.*

object RepeatedParams {
  @WasmImport("env", "imported")
  def imported(x: Int*): Unit = wasm.native // error

  @WasmExport("exported")
  def exported(x: Int*): Unit = ??? // error
}

object DefaultParams {
  @WasmImport("env", "imported")
  def imported(x: Int = 1): Unit = wasm.native // error

  @WasmExport("exported")
  def exported(x: Int = 1): Unit = ??? // error
}
