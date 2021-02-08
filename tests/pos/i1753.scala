abstract class BackendInterface {
  type Symbol >: Null
  type ClassDef >: Null
}

class BTypesFromSymbols[I <: BackendInterface](val int: I) {
  def isRemote(s: int.Symbol) = println("might've been remote")
}

trait BCodeIdiomatic {
  val int: BackendInterface
  final lazy val bTypes = new BTypesFromSymbols[int.type](int)
}

trait BCodeSkelBuilder extends BCodeIdiomatic {
  import int.*
  import bTypes.*
  val b: BTypesFromSymbols[int.type] = bTypes
  val x: int.type = bTypes.int
  val y: bTypes.int.type = int
  def getPlainClass(cd: ClassDef) = bTypes.isRemote(null: Symbol)
}
