import java.util.Arrays.asList
import tastymima.intf._

object TastyMiMaFilters {
  val StdlibBootstrapped: java.util.List[ProblemMatcher] = asList(
    // OK
    ProblemMatcher.make(ProblemKind.MissingClass, "scala.*.<local child>"),

    // OK: constructors have a result type the return Unit instead of the class type
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.*.<init>"), // scala.math.Numeric.CharIsIntegral.<init>; before: (): scala.math.Numeric.CharIsIntegral; after: (): Unit

    // Probably OK
    ProblemMatcher.make(ProblemKind.InternalError, "scala.*"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*$extension"),
    ProblemMatcher.make(ProblemKind.IncompatibleSelfTypeChange, "scala.*"),

    // Probably OK: object singleton type
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.BitSet.bitSetFactory"),

    // Probably OK: by-name arguments in signatures
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.App.delayedInit"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.Array.fill"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.*.fill"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.*.getOrElse"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.*.getOrElseUpdate"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.jdk.Accumulator.fill"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.Option.*"), // fold, toLeft, toRight, unless, when
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.Option.getOrElse"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.Option.orElse"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.Predef.*"), // assert, assume, require, Ensuring.ensuring
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.*.getOrElse"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.*.orElse"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.Try.apply"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.Using.apply"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.Using.resources"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.concurrent.Future.*"), // apply, delegate
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.Console.*"), // withErr, withIn, withOut
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.DelayedInit.delayedInit"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.io.Codec.wrap"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.PropertiesTrait.*"), // envOrElse, envOrSome, propOrElse, scalaPropOrElse
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.Either.cond"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.Either.filterOrElse"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.control.Breaks.*"), // breakable, TryBlock.catchBreak, tryBreakable
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.control.Exception.Catch.*"), // andFinally, apply,either, opt, withTry
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.control.Exception.*"), // failAsValue, Finally.and, ultimately
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.control.TailCalls.tailcall"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.DynamicVariable.withValue"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.FileProcessLogger.*"), // buffer, err, out
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessImpl.Spawn.apply"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.processInternal.*"), // onInterrupt, onIOInterrupt
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessLogger.*"), // buffer, err, out
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.PropImpl.or"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.ShutdownHookThread.apply"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.SystemProperties.*"), // exclusively, wrapAccess
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.LazyList.*"), // cons.apply, continually, Deferrer.#::, iterate, lazyAppendedAll, toDeferrer
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.Stream.*"), // append, cons.apply, continually, lazyAppendedAll, toDeferrer
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.Iterator.*"), // ++, concat, continually, GroupedIterator.withPadding
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.concurrent.BlockContext.*"), // blockOn, withBlockContext
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessBuilder.FileBuilder.#<<"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessBuilderImpl.FileImpl.#<<"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessBuilder.Sink.#<"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessBuilder.Source.#>"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessCreation.apply"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.concurrent.BatchingExecutorStatics.MissingParentBlockContext.blockOn"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessImpl.CompoundProcess.runInterruptible"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.util.hashing.Hashing.fromFunction"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.IterableOnceOps.aggregate"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.package.addShutdownHook"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.HashTable.init"),

    // Probably OK: Case class with varargs
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.StringContext.parts"), // before: scala.<repeated>[Predef.String]; after: scala.collection.immutable.Seq[Predef.String] @scala.annotation.internal.Repeated

    // Probably OK: default parameter
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*$default$*"),

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

    // Problem: implicit class (method should not be final)
    ProblemMatcher.make(ProblemKind.FinalMember, "scala.collection.convert.*.*"),

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
  )
}
