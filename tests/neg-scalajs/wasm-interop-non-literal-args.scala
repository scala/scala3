import scala.scalajs.wasm
import scala.scalajs.wasm.annotation.*

object A {
  val module = "env"
  val function = "f"

  @WasmImport(module, "f") // error
  def imported1(): Unit = wasm.native

  @WasmImport("env", function) // error
  def imported2(): Unit = wasm.native
}

object B {
  val exportName = "f"

  @WasmExport(exportName) // error
  def exported(): Unit = ???
}

object InvalidUTF16 {
  @WasmImport("\uD800", "f") // error
  def imported1(): Unit = wasm.native

  @WasmImport("env", "\uD800") // error
  def imported2(): Unit = wasm.native

  @WasmExport("\uD800") // error
  def exported(): Unit = ???
}
