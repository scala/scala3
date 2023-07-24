import java.util.Arrays.asList
import tastymima.intf._

object TastyMiMaFilters {
  val StdlibBootstrapped: java.util.List[ProblemMatcher] = asList(
    // Probably OK
    ProblemMatcher.make(ProblemKind.IncompatibleSelfTypeChange, "scala.*"),

    // Probably OK: Case class with varargs
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.StringContext.parts"), // before: scala.<repeated>[Predef.String]; after: scala.collection.immutable.Seq[Predef.String] @scala.annotation.internal.Repeated

    // Probably OK: ConstantType for `null` versus `scala.Null`
    // Calls to the default getter seem to link correctly.
    // Tested in stdlib-bootstrapped/test/scala/collection/UnrolledBufferTest.scala
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.UnrolledBuffer.Unrolled.<init>$default$4"),

    // Problem? Very complicated signature
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.generic.IsMap.mapOpsIsMap"),

    // Probably OK: Overriding java method (`public abstract Object underlying();` with `def underlying: Object`)
    // Calls to the underlying seem to link correctly.
    // Tested in stdlib-bootstrapped/test/Main.scala
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.math.Big*.underlying"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.math.ScalaNumericConversions.underlying"),

    // Problem: super accessors
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.*.superscala$*$*$$*"), // The member scala.collection.mutable.Cloneable.superscala$collection$mutable$Cloneable$$clone was concrete or did not exist but is abstract in current version

    // Problem: `private[scala] var` in case class
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.::.next$access$1"),

    // Problem Missing setter for `protected var`
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.*_="),

    // Problem: ???
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.JavaCollectionWrappers.IterableWrapperTrait.iterator"),

    // TASTy-MiMa bugs
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.appendedAll"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.concat"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.prependedAll"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.concurrent.duration.package.*"),

    // Problems introduced in 2.13.11
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.BuildFromLowPriority1.buildFromSortedSetOps"), // The symbol scala.collection.BuildFromLowPriority1.buildFromSortedSetOps has an incompatible type in current version: before: [CC <: ([X] =>> (scala.collection.SortedSet[X] & scala.collection.SortedSetOps[X, CC, ?])), A0, A](evidence$3: scala.package.Ordering[A])scala.collection.BuildFrom[(CC[A0] & scala.collection.SortedSet[A0]), A, (CC[A] & scala.collection.SortedSet[A])]; after: [CC >: ([X] =>> scala.Nothing) <: ([X] =>> scala.&[scala.collection.SortedSet[X], scala.collection.SortedSetOps[X, CC, ?]]), A0, A](evidence$3: scala.package.Ordering[A])scala.collection.BuildFrom[scala.&[CC[A0], scala.collection.SortedSet[A0]], A, scala.&[CC[A], scala.collection.SortedSet[A]]]
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.BuildFrom.buildFromMapOps"), // The symbol scala.collection.BuildFrom.buildFromMapOps has an incompatible type in current version: before: [CC <: ([X, Y] =>> (scala.collection.Map[X, Y] & scala.collection.MapOps[X, Y, CC, ?])), K0, V0, K, V]scala.collection.BuildFrom[(CC[K0, V0] & scala.collection.Map[K0, V0]), scala.Tuple2[K, V], (CC[K, V] & scala.collection.Map[K, V])]; after: [CC >: ([X, Y] =>> scala.Nothing) <: ([X, Y] =>> scala.&[scala.collection.Map[X, Y], scala.collection.MapOps[X, Y, CC, ?]]), K0, V0, K, V]scala.collection.BuildFrom[scala.&[CC[K0, V0], scala.collection.Map[K0, V0]], scala.Tuple2[K, V], scala.&[CC[K, V], scala.collection.Map[K, V]]]
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.BuildFrom.buildFromSortedMapOps"), // The symbol scala.collection.BuildFrom.buildFromSortedMapOps has an incompatible type in current version: before: [CC <: ([X, Y] =>> (scala.collection.SortedMap[X, Y] & scala.collection.SortedMapOps[X, Y, CC, ?])), K0, V0, K, V](evidence$1: scala.package.Ordering[K])scala.collection.BuildFrom[(CC[K0, V0] & scala.collection.SortedMap[K0, V0]), scala.Tuple2[K, V], (CC[K, V] & scala.collection.SortedMap[K, V])]; after: [CC >: ([X, Y] =>> scala.Nothing) <: ([X, Y] =>> scala.&[scala.collection.SortedMap[X, Y], scala.collection.SortedMapOps[X, Y, CC, ?]]), K0, V0, K, V](evidence$1: scala.package.Ordering[K])scala.collection.BuildFrom[scala.&[CC[K0, V0], scala.collection.SortedMap[K0, V0]], scala.Tuple2[K, V], scala.&[CC[K, V], scala.collection.SortedMap[K, V]]]
  )
}
