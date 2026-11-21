import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.wasm
import scala.scalajs.wasm.annotation.*

object A {
  @js.native @JSGlobal
  @WasmImport("env", "imported") // error
  def imported(): Unit = js.native

  @js.native @JSGlobal
  @WasmExport("exported") // error
  def exported(): Unit = ??? // error
}
