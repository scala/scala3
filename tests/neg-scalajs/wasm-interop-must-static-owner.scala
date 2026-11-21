import scala.scalajs.js
import scala.scalajs.wasm
import scala.scalajs.wasm.annotation.*

class ClassOwner {
  @WasmImport("env", "imported")
  def imported(): Unit = wasm.native // error

  @WasmExport("exported")
  def exported(): Unit = ??? // error
}

class EnclosingClass {
  object A {
    @WasmImport("env", "imported")
    def imported(): Unit = wasm.native // error

    @WasmExport("exported")
    def exported(): Unit = ??? // error
  }
}

object EnclosingObject {
  object A {
    @WasmImport("env", "imported")
    def imported(): Unit = wasm.native // ok

    @WasmExport("exported")
    def exported(): Unit = ??? // ok
  }
}

object JSOwner extends js.Object {
  @WasmImport("env", "imported")
  def imported(): Unit = wasm.native // error

  @WasmExport("exported")
  def exported(): Unit = ??? // error
}

object LocalOwner {
  def outer(): Unit = {
    @WasmImport("env", "imported")
    def imported(): Unit = wasm.native // error

    @WasmExport("exported")
    def exported(): Unit = ??? // error
  }
}
