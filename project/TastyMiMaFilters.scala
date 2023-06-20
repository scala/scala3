import java.util.Arrays.asList
import tastymima.intf._

object TastyMiMaFilters {
  val StdlibBootstrapped: java.util.List[ProblemMatcher] = asList(
    // OK: constructors have a result type the return Unit instead of the class type
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.*.<init>"), // scala.math.Numeric.CharIsIntegral.<init>; before: (): scala.math.Numeric.CharIsIntegral; after: (): Unit

    // Probably OK
    ProblemMatcher.make(ProblemKind.IncompatibleSelfTypeChange, "scala.*"),

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
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.UnrolledBuffer.Unrolled.<init>$default$4"),

    // Problem: Case class with private constructor
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.concurrent.duration.Deadline.apply"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.concurrent.duration.Deadline.copy"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.concurrent.duration.Deadline.copy$default$1"),

    // Problem: Missing type arguments with higher-kinded types
    ProblemMatcher.make(ProblemKind.MissingTypeMember, "scala.collection.SortedSetFactoryDefaults._$5"),
    ProblemMatcher.make(ProblemKind.MissingTypeMember, "scala.collection.SortedMapFactoryDefaults._$6"),

    // Problem: Incompatible type change of higher-kinded types
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.*CC"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.*.C"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.jdk.Accumulator.CC"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.EvidenceIterableFactory*.Ev"),

    // Problem: Refined type in signature
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.generic.IsMap.Tupled"), // scala.collection.generic.IsMap.Tupled; source: type Tupled[F[+_]] = { type Ap[X, Y] = F[(X, Y)] }; before: [F] =>> Any; after: [F] =>> { type Ap = [X, Y] =>> F[(X,Y)]}
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsMap.*IsMap"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsSeq.*IsSeq"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.runtime.ScalaRunTime.drop"),

    // Problem: Overriding java method (`public abstract Object underlying();` with `def underlying: Object`)
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.math.Big*.underlying"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.math.ScalaNumericConversions.underlying"),

    // Problem: Inferred result type of non-private member differs
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.JavaCollectionWrappers.IterableWrapperTrait.iterator"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.matching.Regex.MatchIterator.replacementData"), // before: scala.Any; after: scala.collection.AbstractIterator[scala.util.matching.Regex] & scala.util.matching.Regex.Replacement
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.util.hashing.Hashing.fromFunction"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.UnrolledBuffer.classTagCompanion"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.JavaCollectionWrappers.SetWrapper.iterator"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.concurrent.FailedNode.string"),

    // Problem: implicit class
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterableLowPriority.is*LikeIsIterable"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterableOnce.iterableOnceIsIterableOnce"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterableOnceLowPriority.isIterableLikeIsIterableOnce"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterable.*OpsIsIterable"),

    // Problem: super accessors
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.*.superscala$*$*$$*"), // The member scala.collection.mutable.Cloneable.superscala$collection$mutable$Cloneable$$clone was concrete or did not exist but is abstract in current version

    // Problem: `private[scala] var` in case class
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.::.next$access$1"),

    // Problem Missing setter for `protected var`
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.*_="),

    // Problem: type projection? Can we just inline the type?
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.generic.IsMap.apply"), // before: (c: IsMap.this.Repr)scala.collection.MapOps[IsMap.this.K, IsMap.this.V, ([X, Y] =>> scala.collection.Iterable[scala.Tuple2[X, Y]]), IsMap.this.C]; after: (c: IsMap.this.Repr)scala.collection.MapOps[IsMap.this.K, IsMap.this.V, scala.collection.generic.IsMap.Tupled[([A] =>> scala.collection.Iterable[A])]#Ap, IsMap.this.C]

    // Problem: problem with different inherited versions of the operations?
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.StrictOptimizedSortedMapOps.concat"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.SortedMapOps.++"), // before: [V2 >: SortedMapOps.this.V](xs: scala.collection.IterableOnce[scala.Tuple2[SortedMapOps.this.K, V2]])SortedMapOps.this.CC[SortedMapOps.this.K, V2]; after: [B >: scala.Tuple2[SortedMapOps.this.K, SortedMapOps.this.V]](suffix: scala.collection.IterableOnce[B])([A] =>> scala.collection.Iterable[A])[B]
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.SortedMapOps.concat"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.StrictOptimizedSortedMapOps.concat"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.SortedMapOps.+"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.SortedMapOps.transform"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.SortedMapOps.updated"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.SortedMapOps.updatedWith"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.+"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.collect"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.flatMap"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.map"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.sortedMapFromIterable"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.WithFilter.flatMap"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.SortedMapOps.WithFilter.map"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StrictOptimizedSortedMapOps.+"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StrictOptimizedSortedMapOps.collect"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StrictOptimizedSortedMapOps.flatMap"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StrictOptimizedSortedMapOps.map"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.immutable.SortedMapOps.updated"),
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
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.mutable.SortedMapOps.updated"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.SortedSetFactoryDefaults.fromSpecific"), // before: (coll: scala.collection.IterableOnce[SortedSetFactoryDefaults.this.A])SortedSetFactoryDefaults.this.CC[SortedSetFactoryDefaults.this.A]; after: (coll: scala.collection.IterableOnce[(@<annot> (SortedSetFactoryDefaults.this.A & (@<annot> SortedSetFactoryDefaults.this.A)))])(scala.collection.IterableOps[SortedSetFactoryDefaults.this.A, SortedSetFactoryDefaults.this.WithFilterCC, ?]#C & SortedSetFactoryDefaults.this.CC[(@<annot> SortedSetFactoryDefaults.this.A)])
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.SortedMapFactoryDefaults.fromSpecific"),

    // TASTy-MiMa bug? Incompatible type change is `with` intersection types
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.convert.impl.*.Semi"), // scala.collection.convert.impl.BinaryTreeStepperBase.Semi; source: Semi <: Sub with BinaryTreeStepperBase[A, T, _, _]; before: _ :> scala.Nothing <: scala.Any; after: :> scala.Nothing <: scala.collection.convert.impl.BinaryTreeStepperBase.Sub & scala.collection.convert.impl.BinaryTreeStepperBase[scala.collection.convert.impl.BinaryTreeStepperBase.A, scala.collection.convert.impl.BinaryTreeStepperBase.T, _ :> scala.Nothing <: scala.Any, _ :> scala.Nothing <: scala.Any]
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.*MapOps.coll"), // scala.collection.immutable.MapOps.coll; source: C with CC[K, V]; before: scala.Any; after: scala.&[scala.collection.immutable.MapOps.C, scala.collection.immutable.MapOps.CC[scala.collection.immutable.MapOps.K, scala.collection.immutable.MapOps.V]]
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.package.LinearSeq"), // before: [X] =>> Any; after:  [X] ==> scala.&[scala.collection.mutable.Seq[X], scala.collection.LinearSeq[X]]
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.AnyStepper.ofPar*Stepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.stepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.StepperShape.parUnbox"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.runtime.Tuple2Zipped.Ops.zipped"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.runtime.Tuple3Zipped.Ops.zipped"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.impl.BitSetStepper.from"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.*mutable.*Map.valueStepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.*mutable.*Map.keyStepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.StreamExtensions.StepperHasParStream"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.efficientStepper"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.jdk.Accumulator.efficientStepper"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.package.:+.unapply"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.package.+:.unapply"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.impl.*StepperBase.semiclone"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.*StepperBase.semiclone"),

    // TASTy-MiMa bug? Wildcards in self type
    ProblemMatcher.make(ProblemKind.MissingTypeMember, "scala.collection.generic.DefaultSerializable._$1"),

    // TASTy-MiMa bug? module classes
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.BitSet.bitSetFactory"), // The symbol scala.collection.immutable.BitSet.bitSetFactory has an incompatible type in current version: before: scala.collection.immutable.BitSet$; after: scala.collection.immutable.BitSet.type
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.BitSet.bitSetFactory"), // The symbol scala.collection.mutable.BitSet.bitSetFactory has an incompatible type in current version: before: scala.collection.mutable.BitSet$; after: scala.collection.mutable.BitSet.type

    // TASTy-MiMa bugs
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.appendedAll"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.concat"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.collection.SeqView.prependedAll"),
    ProblemMatcher.make(ProblemKind.InternalError, "scala.concurrent.duration.package.*"),
  )
}
