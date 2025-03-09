package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import StdNames.nme
import ast.tpd.*
import Decorators.*
import typer.ErrorReporting.errorType
import Names.TermName
import NameKinds.ExistentialBinderName
import NameOps.isImpureFunction
import reporting.Message
import util.SimpleIdentitySet.empty
import CaptureSet.{Refs, emptyRefs, NarrowingCapabilityMap}
import dotty.tools.dotc.util.SimpleIdentitySet

/** A module for handling Fresh types. Fresh instances are top types that keep
 *  track of what they hide when capabilities get widened by subsumption to fresh.
 *  The module implements operations to convert between regular caps.cap and
 *  Fresh instances. root.Fresh(...) is encoded as `caps.cap @freshCapability(...)` where
 *  `freshCapability(...)` is a special kind of annotation of type `root.Annot`
 *  that contains a hidden set.
 */
object Fresh