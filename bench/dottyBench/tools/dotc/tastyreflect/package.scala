package dottyBench.tools.dotc

import dottyBench.tools.dotc.ast.Trees.{Tree, Untyped}
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.core.Symbols.Symbol
import dottyBench.tools.dotc.core.Types.Type
import dottyBench.tools.dotc.core.SymDenotations.SymDenotation
import scala.annotation.constructorOnly
import util.SourceFile

package object tastyreflect {

  type PackageDefinition = PackageDefinitionImpl[Type]

  /** Represents the symbol of a definition in tree form */
  case class PackageDefinitionImpl[-T >: Untyped] private[tastyreflect] (sym: Symbol)(implicit @constructorOnly src: SourceFile) extends Tree[T] {
    type ThisTree[-T >: Untyped] = PackageDefinitionImpl[T]

    override def denot(using Ctx, CState): SymDenotation = sym.denot
  }
}

