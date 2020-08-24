package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.Trees.{Tree, Untyped}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import scala.annotation.constructorOnly

package object reflect {

  type PackageDefinition = PackageDefinitionImpl[Type]

  /** Represents the symbol of a definition in tree form */
  case class PackageDefinitionImpl[-T >: Untyped] private[reflect] (sym: Symbol)(implicit @constructorOnly src: SourceFile) extends Tree[T] {
    type ThisTree[-T >: Untyped] = PackageDefinitionImpl[T]

    override def denot(using Context): SymDenotation = sym.denot
  }
}

