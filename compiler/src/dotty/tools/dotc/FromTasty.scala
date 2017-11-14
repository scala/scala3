/* dotc
 * Copyright 2005-2015 LAMP/EPFL
 * @author  Martin Odersky
 */
package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.fromtasty.TASTYCompiler

/** Compiler for TASTY files.
 *  Usage:
 *
 *    scala dotty.tools.dotc.FromTasty (option | classname)*
 *
 *  Options are as for dotc.
 *  Classnames are fully qualified names of top-level classes that need to have a TASTY attribute.
 *  Example:
 *
 *    scala dotty.tools.dotc.FromTasty -Xprint:front extMethods.T
 */
object FromTasty extends Driver {
  override def newCompiler(implicit ctx: Context): Compiler = new TASTYCompiler
}
