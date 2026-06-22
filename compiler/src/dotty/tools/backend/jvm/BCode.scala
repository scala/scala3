package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd.TypeDef
import dotty.tools.dotc.core.Contexts.*

import dotty.tools.backend.ScalaPrimitives

import scala.tools.asm.tree.ClassNode

/** Interface to the series of `BCode*` traits. */
final class BCode(knownBTypes: KnownBTypes, bTypeLoader: BTypeLoader, primitives: ScalaPrimitives, callGraph: CallGraph):

  private object impl extends BCodeIdiomatic(callGraph), BCodeSkelBuilder(knownBTypes), BCodeHelpers(bTypeLoader), BCodeBodyBuilder(primitives), BCodeSyncAndTry

  // The non-mirror builder cannot be shared as it includes per-class mutable state that is not reset
  private val mirrorBuilder = new impl.JMirrorBuilder()

  /** Generates a ClassNode for the given type definition. */
  def genClassNode(typeDef: TypeDef)(using Context): ClassNode =
    new impl.SyncAndTryBuilder().genPlainClass(typeDef, topLevel = true)

  /** Generates a mirror ClassNode for the given type definition, if necessary. */
  def genMirrorClassNode(typeDef: TypeDef)(using Context): Option[ClassNode] =
    Option(mirrorBuilder.genMirrorClassIfNeeded(typeDef.symbol))

  /** Gets the cache for ClassBTypes used by the bytecode generator. */
  def classBTypeCache(): ClassBType.Cache =
    bTypeLoader.cache
