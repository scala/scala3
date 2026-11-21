import scala.scalajs.wasm
import scala.scalajs.wasm.annotation.*

object TypeParams {
  @WasmImport("env", "imported")
  def imported[A](x: Int): Unit = wasm.native // error

  @WasmExport("exported")
  def exported[A](x: Int): Unit = ??? // error
}

object MultiParams {
  @WasmImport("env", "imported")
  def imported(x: Int)(y: Int): Unit = wasm.native // error

  @WasmExport("exported")
  def exported(x: Int)(y: Int): Unit = ??? // error
}

object NoParams {
  @WasmImport("env", "imported")
  def imported: Int = wasm.native // error

  @WasmExport("exported")
  def exported: Int = ??? // error
}
