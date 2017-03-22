package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._

/** Suffix encoding M of `PhantomFunctionM` and `ImplicitPhantomFunctionM`
 *
 *  `FunctionN` and `ImplicitFunctionN` can also be encoded as
 *  `PhantomFunctionM` and `ImplicitPhantomFunctionM` where `M` has `N + 1` `O`s
 */
class Phantomicity private (val encodedNames: Array[TermName]) extends AnyVal {
  import Phantomicity._

  /** If has phantom parameters or return type */
  def hasPhantoms: Boolean = encodedNames.exists(_ != scalaLattice)

  /** If all parameters and return type are phantoms */
  def allPhantoms: Boolean = encodedNames.forall(_ != scalaLattice)

  /** If the return type is a phantom */
  def returnsPhantom: Boolean = encodedNames.last != scalaLattice

  /** Arity of this function. Including phantom parameters. */
  def arity: Int = if (isValid) encodedNames.length - 1 else -1

  /** Erased arity of this function. Without phantom parameters. */
  def erasedArity: Int =
    if (isValid) encodedNames.init.count(_ == scalaLattice)
    else -1

  def tParamBounds(i: Int)(implicit ctx: Context): TypeBounds = {
    val latticeName = encodedNames(i).decode
    if (latticeName == scalaLattice) {
      TypeBounds.empty
    } else {
      val lattice = staticRefOf(latticeName).info
      TypeBounds(lattice.select(tpnme.Nothing), lattice.select(tpnme.Any))
    }
  }

  /** Is a valid phantomicity */
  def isValid: Boolean = encodedNames ne invalid.encodedNames

  /** Encoded suffix of the function name */
  def encodedString: String = "$$" + encodedNames.mkString(separator.toString)

}

object Phantomicity {
  private val separator = '_'
  private val scalaLattice = "scala".toTermName

  lazy val invalid: Phantomicity = new Phantomicity(Array.empty)

  def apply(types: List[Type])(implicit ctx: Context): Phantomicity = {
    def typeToString(tp: Type) = {
      val lattice = tp.phantomLatticeClass
      if (lattice.exists) lattice.termSymbol.fullName.encode.asTermName
      else scalaLattice
    }
    val encodedStrings = types.iterator.map(typeToString).toArray
    new Phantomicity(encodedStrings)
  }

  def from(str: String): Phantomicity =
    new Phantomicity(str.substring(2).split(separator).map(_.toTermName))

  def noPhantoms(arity: Int)(implicit ctx: Context): Phantomicity = apply(List.fill(arity + 1)(defn.AnyType))

  private def staticRefOf(name: Name)(implicit ctx: Context) =
    if (name.contains('.')) ctx.base.staticRef(name)
    else defn.EmptyPackageClass.info.decl(name)
}
