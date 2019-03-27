package dotty.tools.dotc.typer

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.TypeBounds
import dotty.tools.dotc.typer.ProtoTypes.newTypeVar
import org.junit.Test

class SubtypingInvariantTests extends DottyTest {

  @Test
  def typeVarInvariant(): Unit = {
    checkCompile("frontend", "class A") { (_, ctx0) =>
      implicit val ctx: Context = ctx0
      val a = newTypeVar(TypeBounds.empty)
      val b = newTypeVar(TypeBounds.empty)
      assert(a <:< b)
      assert(a frozen_<:< b)
    }
  }

}
