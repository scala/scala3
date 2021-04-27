trait CompilerInterface {
  type Tree22
}

class Reflect(val internal: CompilerInterface) {
  opaque type Tree22 = internal.Tree22
  def show(t: Tree22): String = ???
}

object App {
  val refl: Reflect = ???
  import refl.*

  show(??? : Tree22)
}