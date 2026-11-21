import scala.scalajs.wasm
import scala.scalajs.wasm.annotation.*

object Private {
  @WasmImport("env", "imported")
  private def imported(): Unit = wasm.native // error

  @WasmExport("exported")
  private def exported(): Unit = ??? // error
}

object Protected {
  @WasmImport("env", "imported")
  protected def imported(): Unit = wasm.native // error

  @WasmExport("exported")
  protected def exported(): Unit = ??? // error
}

object PackagePrivate {
  @WasmImport("env", "imported")
  private[PackagePrivate] def imported(): Unit = wasm.native // error

  @WasmExport("exported")
  private[PackagePrivate] def exported(): Unit = ??? // error
}
