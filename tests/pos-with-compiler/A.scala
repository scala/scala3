import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.NameOps.*

object Test {
  "JFunction".toTermName.specializedFor(Vector.empty, ???, Vector.empty, Vector.empty)(using ???)
}
