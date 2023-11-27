package dotty.tools.dotc.core

import dotty.tools.io.AbstractFile
import dotty.tools.tasty.TastyVersion

/** Information about the TASTy of a class symbol.
 *
 *  @param version     The TASTy version (major, minor, experimental)
 *  @param attributes  Attributes of in the TASTy attributes section
 */
case class TastyInfo(version: TastyVersion, attributes: tasty.Attributes)
