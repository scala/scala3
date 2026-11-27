import scala.scalajs.wasm
import scala.scalajs.wasm.annotation.*

class ImportConstructor @WasmImport("env", "ctor") () // error
class ExportConstructor @WasmExport("ctor") () // error

object A {
  @WasmImport("env", "x")
  val imported: Int = 1 // error

  @WasmExport("x")
  val exported: Int = 1 // error
}
