package xsbt

import xsbt.api.SameAPI
import xsbti.api.{ClassDefinition, ClassLike, DefinitionType}

/**
 * Simulates the core Zinc name-hashing invalidation decision without running a
 * full sbt build.
 *
 * The real Zinc algorithm (simplified):
 *  1. After compiling upstream_v1 and downstream together, record:
 *       - the declared API of every class produced by upstream_v1
 *       - all names used by downstream classes
 *  2. Recompile upstream (v2 only) to obtain the new declared API.
 *  3. "Changed names" = names whose declared-member set differs between v1 and v2,
 *     detected with `SameAPI` (structural comparison, not hash values).
 *  4. downstream is invalidated iff it used any changed name.
 *
 * ## Why SameAPI rather than NameHashing.nameHashes?
 *
 * `NameHashing.nameHashes` produces a single integer hash per (name, scope) pair
 * that encodes the *entire* inherited class structure via `HashAPI`.  Between two
 * separate compiler runs on identical source the inherited-member order can differ
 * (it depends on internal hash-map iteration order), making the aggregate hash
 * non-deterministic.  `SameAPI` instead performs a field-by-field structural
 * comparison and is stable across runs.
 *
 * The trade-off is that we compare only `structure.declared` (members explicitly
 * declared in this class), not the full inherited structure.  That is the same
 * granularity real Zinc uses when deciding which *names* changed: a name is changed
 * when the declarations that carry it differ, regardless of what is inherited.
 */
class IncrementalCompileSimulator:
  private val compiler = new ScalaCompilerForUnitTesting

  /**
   * Returns true if `downstream` would be scheduled for recompilation when
   * `upstream` changes from `v1` to `v2`.
   *
   * v1 and downstream are compiled together in one run (so downstream can
   * refer to definitions in upstream).  v2 is compiled in isolation to obtain
   * the new upstream API.
   */
  def wouldRecompile(upstreamV1: String, downstream: String, upstreamV2: String): Boolean =
    val changed = changedNamesBetween(upstreamV1, upstreamV2)
    val used    = usedNamesIn(upstreamV1, downstream)
    changed.exists(used.contains)

  // ── private ──────────────────────────────────────────────────────────────

  /** Names whose declared-member set differs between the two upstream versions. */
  private def changedNamesBetween(v1: String, v2: String): Set[String] =
    val byKeyV1 = classesByKey(apiOf(v1))
    val byKeyV2 = classesByKey(apiOf(v2))
    (byKeyV1.keySet ++ byKeyV2.keySet).flatMap { key =>
      (byKeyV1.get(key), byKeyV2.get(key)) match
        case (Some(c1), Some(c2)) => memberChangedNames(c1, c2)
        case (None,     Some(c2)) => c2.structure.declared.map(_.name).toSet
        case (Some(c1), None    ) => c1.structure.declared.map(_.name).toSet
        case _                    => Set.empty
    }

  /**
   * All names used by downstream classes when downstream is compiled together
   * with upstream.
   */
  private def usedNamesIn(upstream: String, downstream: String): Set[String] =
    val output          = compiler.compileSrcs(upstream, downstream)
    val downstreamFile  = output.srcFiles(1)
    val downstreamClasses = output.analysis.classNames(downstreamFile).map(_._1)
    downstreamClasses.flatMap(cls => output.analysis.usedNames(cls)).toSet

  /** API produced by compiling a single source, as a sequence of ClassLikes. */
  private def apiOf(src: String): Seq[ClassLike] =
    val output = compiler.compileSrcs(src)
    output.srcFiles.flatMap(f => output.analysis.apis(f))

  /**
   * Index ClassLikes by (name, definitionType).
   *
   * The name alone is insufficient: ExtractAPI strips the `$` suffix from module
   * class names, so a case class C and its companion object C$ both report the name
   * "C".  Including the DefinitionType (ClassDef vs Module) makes the key unique.
   */
  private def classesByKey(classes: Seq[ClassLike]): Map[(String, DefinitionType), ClassLike] =
    classes.map(c => (c.name, c.definitionType) -> c).toMap

  /**
   * Names of members that differ in their declared definitions between two
   * versions of the same class.
   *
   * Groups `structure.declared` by name and uses `SameAPI` for structural
   * comparison, so the result is independent of member-list ordering.
   */
  private def memberChangedNames(c1: ClassLike, c2: ClassLike): Set[String] =
    val byNameV1 = c1.structure.declared.groupBy(_.name)
    val byNameV2 = c2.structure.declared.groupBy(_.name)
    (byNameV1.keySet ++ byNameV2.keySet).filter { name =>
      !sameDefinitions(
        byNameV1.getOrElse(name, Array.empty),
        byNameV2.getOrElse(name, Array.empty)
      )
    }

  /**
   * True iff two arrays of definitions for the same name are pairwise equal
   * under `SameAPI`.  Arrays are sorted by name before comparison so that
   * non-deterministic ordering within an overload group does not produce false
   * positives.
   */
  private def sameDefinitions(ds1: Array[ClassDefinition], ds2: Array[ClassDefinition]): Boolean =
    if ds1.length != ds2.length then false
    else
      val s1 = ds1.sortBy(_.name)
      val s2 = ds2.sortBy(_.name)
      s1.zip(s2).forall((d1, d2) => SameAPI(d1, d2))
