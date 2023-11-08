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
    // Tested in scala2-library-bootstrapped/test/scala/collection/UnrolledBufferTest.scala
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.UnrolledBuffer.Unrolled.<init>$default$4"),

    // Probably OK: Overriding java method (`public abstract Object underlying();` with `def underlying: Object`)
    // Calls to the underlying seem to link correctly.
    // Tested in scala2-library-bootstrapped/test/Main.scala
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.math.Big*.underlying"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.math.ScalaNumericConversions.underlying"),

    // Problem: super accessors
    // In Scala 3 these accessors are added in the `postyper` phase.
    // In Scala 2 these accessors are added in the `superaccessors` phase after typer.
    // Are these accessors in the Scala 2 pickles? If so, it implies that TASTy Query/MiMa is ignoring them in Scala 2 but not Scala 3.
    // Otherwise, if these are not in the Scala 2 pickles, we might need to remove them when compiling with -Ycompile-scala2-library
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.immutable.IndexedSeqOps.superscala$collection$immutable$IndexedSeqOps$$slice"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.immutable.StrictOptimizedSeqOps.superscala$collection$immutable$StrictOptimizedSeqOps$$sorted"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.immutable.IndexedSeq.superscala$collection$immutable$IndexedSeq$$*"/* sameElements, canEqual */),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.SortedSetOps.superscala$collection$SortedSetOps$$*"/* min, max */),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.SortedSet.superscala$collection$SortedSet$$equals"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.LinearSeqOps.superscala$collection$LinearSeqOps$$sameElements"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.SortedMap.superscala$collection$SortedMap$$equals"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.SeqOps.superscala$collection$SeqOps$$*"/* concat, sizeCompare */),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.BitSetOps.superscala$collection$BitSetOps$$*"/* min, intersect, concat, diff, max */),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.mutable.Cloneable.superscala$collection$mutable$Cloneable$$clone"), // The member scala.collection.mutable.Cloneable.superscala$collection$mutable$Cloneable$$clone was concrete or did not exist but is abstract in current version
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.util.control.NoStackTrace.superscala$util$control$NoStackTrace$$fillInStackTrace"),

    // TASTy-MiMa bug (probably OK): `private[scala] var` in case class
    // This is probably because we can only access the next field from the scala library.
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.::.next$access$1"),

    // Probably OK: Problem Missing setter for `protected var`
    // All the classes that contain these `protected var`s are private in `collection` or `convert`
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.BinaryTreeStepperBase.index_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.BinaryTreeStepperBase.myCurrent_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.BinaryTreeStepperBase.maxLength_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.BinaryTreeStepperBase.stack_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.ChampStepperBase.maxSize_="), // The member scala.collection.convert.impl.ChampStepperBase.maxSize_= with signature (scala.Int):scala.Unit was concrete or did not exist but is abstract in current version
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.IndexedStepperBase.iN_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.IndexedStepperBase.i0_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.InOrderStepperBase.iN_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.InOrderStepperBase.i0_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.TableStepperBase.i0_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.TableStepperBase.maxLength_="),

    // Problem: ???
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.Predef.nn"), // The member scala.Predef.nn with signature (1,java.lang.Object):java.lang.Object does not have a correspondant in current version
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.Predef.ne"), // The member scala.Predef.ne with signature (java.lang.Object,java.lang.Object):scala.Boolean does not have a correspondant in current version
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.Predef.eq"), // The member scala.Predef.eq with signature (java.lang.Object,java.lang.Object):scala.Boolean does not have a correspondant in current version

    // Problem: protected lazy val (processThread, (futureThread, futureValue), destroyer) = { ... }
    // https://github.com/scala/scala/blob/cff8a9af4da67658d8e1e32f929e1aff03ffa384/src/library/scala/sys/process/ProcessImpl.scala#L99C5-L99C83
    ProblemMatcher.make(ProblemKind.IncompatibleKindChange, "scala.sys.process.ProcessImpl.CompoundProcess.destroyer"), // before: lazy val; after: def
    ProblemMatcher.make(ProblemKind.IncompatibleKindChange, "scala.sys.process.ProcessImpl.CompoundProcess.futureThread"), // before: lazy val; after: def
    ProblemMatcher.make(ProblemKind.IncompatibleKindChange, "scala.sys.process.ProcessImpl.CompoundProcess.processThread"), // before: lazy val; after: def
    ProblemMatcher.make(ProblemKind.IncompatibleKindChange, "scala.sys.process.ProcessImpl.CompoundProcess.futureValue"), // before: lazy val; after: def

    // Problem: ???
    // Member is defined and has explicit result type
    // https://github.com/scala/scala/blob/2.13.x/src/library/scala/collection/convert/JavaCollectionWrappers.scala#L66-L71
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.JavaCollectionWrappers.IterableWrapperTrait.iterator"), // The member scala.collection.convert.JavaCollectionWrappers.IterableWrapperTrait.iterator with signature ():scala.collection.convert.JavaCollectionWrappers.IteratorWrapper does not have a correspondant in current version

    // TASTy-MiMa bugs
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.appendedAll"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.concat"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.prependedAll"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.concurrent.duration.package.*"),

    // Problems introduced in 2.13.11: Implicit classes with complex signatures
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.BuildFromLowPriority1.buildFromSortedSetOps"), // The symbol scala.collection.BuildFromLowPriority1.buildFromSortedSetOps has an incompatible type in current version: before: [CC <: ([X] =>> (scala.collection.SortedSet[X] & scala.collection.SortedSetOps[X, CC, ?])), A0, A](evidence$3: scala.package.Ordering[A])scala.collection.BuildFrom[(CC[A0] & scala.collection.SortedSet[A0]), A, (CC[A] & scala.collection.SortedSet[A])]; after: [CC >: ([X] =>> scala.Nothing) <: ([X] =>> scala.&[scala.collection.SortedSet[X], scala.collection.SortedSetOps[X, CC, ?]]), A0, A](evidence$3: scala.package.Ordering[A])scala.collection.BuildFrom[scala.&[CC[A0], scala.collection.SortedSet[A0]], A, scala.&[CC[A], scala.collection.SortedSet[A]]]
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.BuildFrom.buildFromMapOps"), // The symbol scala.collection.BuildFrom.buildFromMapOps has an incompatible type in current version: before: [CC <: ([X, Y] =>> (scala.collection.Map[X, Y] & scala.collection.MapOps[X, Y, CC, ?])), K0, V0, K, V]scala.collection.BuildFrom[(CC[K0, V0] & scala.collection.Map[K0, V0]), scala.Tuple2[K, V], (CC[K, V] & scala.collection.Map[K, V])]; after: [CC >: ([X, Y] =>> scala.Nothing) <: ([X, Y] =>> scala.&[scala.collection.Map[X, Y], scala.collection.MapOps[X, Y, CC, ?]]), K0, V0, K, V]scala.collection.BuildFrom[scala.&[CC[K0, V0], scala.collection.Map[K0, V0]], scala.Tuple2[K, V], scala.&[CC[K, V], scala.collection.Map[K, V]]]
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.BuildFrom.buildFromSortedMapOps"), // The symbol scala.collection.BuildFrom.buildFromSortedMapOps has an incompatible type in current version: before: [CC <: ([X, Y] =>> (scala.collection.SortedMap[X, Y] & scala.collection.SortedMapOps[X, Y, CC, ?])), K0, V0, K, V](evidence$1: scala.package.Ordering[K])scala.collection.BuildFrom[(CC[K0, V0] & scala.collection.SortedMap[K0, V0]), scala.Tuple2[K, V], (CC[K, V] & scala.collection.SortedMap[K, V])]; after: [CC >: ([X, Y] =>> scala.Nothing) <: ([X, Y] =>> scala.&[scala.collection.SortedMap[X, Y], scala.collection.SortedMapOps[X, Y, CC, ?]]), K0, V0, K, V](evidence$1: scala.package.Ordering[K])scala.collection.BuildFrom[scala.&[CC[K0, V0], scala.collection.SortedMap[K0, V0]], scala.Tuple2[K, V], scala.&[CC[K, V], scala.collection.SortedMap[K, V]]]
  )
}
