import java.util.Arrays.asList
import tastymima.intf._

object TastyMiMaFilters {
  val StdlibBootstrapped: java.util.List[ProblemMatcher] = asList(
    // OK: constructors have a result type the return Unit instead of the class type
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.*.<init>"), // scala.math.Numeric.CharIsIntegral.<init>; before: (): scala.math.Numeric.CharIsIntegral; after: (): Unit

    // Probably OK
    ProblemMatcher.make(ProblemKind.IncompatibleSelfTypeChange, "scala.*"),

    // Probably OK: object singleton type
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.BitSet.bitSetFactory"),

    // Probably OK: Case class with varargs
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.StringContext.parts"), // before: scala.<repeated>[Predef.String]; after: scala.collection.immutable.Seq[Predef.String] @scala.annotation.internal.Repeated

    // Problem: secondary constructors?
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.<init>"),

    // Problem: The symbol scala.*.<init> has a more restrictive visibility qualifier in current version
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.Boolean.<init>"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.Byte.<init>"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.Short.<init>"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.Int.<init>"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.Long.<init>"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.Float.<init>"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.Double.<init>"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.Char.<init>"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.Unit.<init>"),

    // Problem: Missing trait constructor
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.$init$"),

    // Problem: Missing Serializable in companions of serializable classes
    ProblemMatcher.make(ProblemKind.MissingParent, "scala.*$"),

    // Problem: Class[T] or ClassTag[T] with `T` equal to wildcard `_ >: Nothing <: AnyVal` instead of a specific primitive type `T`
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.*.getClass"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.reflect.ManifestFactory.*.runtimeClass"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.*.elemTag"),

    // Problem: Case class with private constructor
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.concurrent.duration.Deadline.apply"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.concurrent.duration.Deadline.copy"),

    // Problem: Missing type arguments with higher-kinded types
    ProblemMatcher.make(ProblemKind.MissingTypeMember, "scala.collection.SortedSetFactoryDefaults._$5"),
    ProblemMatcher.make(ProblemKind.MissingTypeMember, "scala.collection.SortedMapFactoryDefaults._$6"),

    // Problem: Incompatible type change of higher-kinded types
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.*CC"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.*.C"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.jdk.Accumulator.CC"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.EvidenceIterableFactory*.Ev"),

    // Problem: Incompatible type change is `with` intersection types
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.convert.impl.*.Semi"), // scala.collection.convert.impl.BinaryTreeStepperBase.Semi; source: Semi <: Sub with BinaryTreeStepperBase[A, T, _, _]; before: _ :> scala.Nothing <: scala.Any; after: :> scala.Nothing <: scala.collection.convert.impl.BinaryTreeStepperBase.Sub & scala.collection.convert.impl.BinaryTreeStepperBase[scala.collection.convert.impl.BinaryTreeStepperBase.A, scala.collection.convert.impl.BinaryTreeStepperBase.T, _ :> scala.Nothing <: scala.Any, _ :> scala.Nothing <: scala.Any]
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.*MapOps.coll"), // scala.collection.immutable.MapOps.coll; source: C with CC[K, V]; before: scala.Any; after: scala.&[scala.collection.immutable.MapOps.C, scala.collection.immutable.MapOps.CC[scala.collection.immutable.MapOps.K, scala.collection.immutable.MapOps.V]]
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.package.LinearSeq"), // before: [X] =>> Any; after:  [X] ==> scala.&[scala.collection.mutable.Seq[X], scala.collection.LinearSeq[X]]

    // Problem: Refined type in signature
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.generic.IsMap.Tupled"), // scala.collection.generic.IsMap.Tupled; source: type Tupled[F[+_]] = { type Ap[X, Y] = F[(X, Y)] }; before: [F] =>> Any; after: [F] =>> { type Ap = [X, Y] =>> F[(X,Y)]}
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsMap.*IsMap"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsSeq.*IsSeq"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.runtime.ScalaRunTime.drop"),

    // Problem: ???
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.math.Big*.underlying"),

    // Problem: Inferred result type of non-private member differs
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.JavaCollectionWrappers.IterableWrapperTrait.iterator"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.matching.Regex.MatchIterator.replacementData"), // before: scala.Any; after: scala.collection.AbstractIterator[scala.util.matching.Regex] & scala.util.matching.Regex.Replacement

    // Problem: implicit class
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterableLowPriority.is*LikeIsIterable"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterableOnce.iterableOnceIsIterableOnce"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterableOnceLowPriority.isIterableLikeIsIterableOnce"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterable.*OpsIsIterable"),

    // Non-categorized
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.::.next$access$1"),
    ProblemMatcher.make(ProblemKind.MissingTypeMember, "scala.collection.generic.DefaultSerializable._$1"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.*_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.math.ScalaNumericConversions.underlying"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.*.superscala$*$*$$*"),

    ProblemMatcher.make(ProblemKind.InternalError, "scala.concurrent.duration.package.*"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.package.:+.unapply"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.package.+:.unapply"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.SortedMapOps.++"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.SortedMapOps.concat"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.map"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.+"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.collect"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.sortedMapFromIterable"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.flatMap"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.WithFilter.map"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.WithFilter.flatMap"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StrictOptimizedSortedMapOps.collect"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.StrictOptimizedSortedMapOps.concat"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StrictOptimizedSortedMapOps.map"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StrictOptimizedSortedMapOps.+"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StrictOptimizedSortedMapOps.flatMap"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.SortedSetFactoryDefaults.fromSpecific"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.prependedAll"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.appendedAll"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.concat"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.SortedMapFactoryDefaults.fromSpecific"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.AnyStepper.ofParIntStepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.AnyStepper.ofParLongStepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.AnyStepper.ofParDoubleStepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StepperShape.parUnbox"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.BitSetOps.map"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.BitSetOps.collect"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.BitSetOps.diff"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.BitSetOps.intersect"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.BitSetOps.^"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.BitSetOps.concat"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.BitSetOps.fromBitMaskNoCopy"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.BitSetOps.xor"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.BitSetOps.flatMap"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.BitSetOps.fromBitMaskNoCopy"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.runtime.Tuple2Zipped.Ops.zipped"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.runtime.Tuple3Zipped.Ops.zipped"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.SortedMapOps.updated"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.SortedMapOps.updatedWith"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.SortedMapOps.transform"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.SortedMapOps.+"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.immutable.SortedMapOps.updated"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.StrictOptimizedSortedMapOps.concat"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.BitSet.bitSetFactory"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.generic.IsMap.apply"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.impl.BitSetStepper.from"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.impl.*.semiclone"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.*.semiclone"),
    ProblemMatcher.make(ProblemKind.FinalMember, "scala.collection.convert.AsJavaExtensions.*AsJava"),
    ProblemMatcher.make(ProblemKind.FinalMember, "scala.collection.convert.AsScalaExtensions.*AsScala"),
    ProblemMatcher.make(ProblemKind.FinalMember, "scala.collection.convert.StreamExtensions.*"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.StreamExtensions.StepperHasParStream"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.JavaCollectionWrappers.SetWrapper.iterator"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.stepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.valueStepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.keyStepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.efficientStepper"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.jdk.Accumulator.efficientStepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.mutable.SortedMapOps.updated"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.UnrolledBuffer.classTagCompanion"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.UnrolledBuffer.Unrolled.<init>$default$4"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.util.hashing.Hashing.fromFunction"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.concurrent.duration.Deadline.copy$default$1"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.concurrent.FailedNode.string"),
  )
}
