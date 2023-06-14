import java.util.Arrays.asList
import tastymima.intf._

object TastyMiMaFilters {
  val StdlibBootstrapped: java.util.List[ProblemMatcher] = asList(
    // OK
    ProblemMatcher.make(ProblemKind.MissingClass, "scala.*.<local child>"),

    // Probably OK
    ProblemMatcher.make(ProblemKind.InternalError, "scala.*"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*$extension"),
    ProblemMatcher.make(ProblemKind.IncompatibleSelfTypeChange, "scala.*"),

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

    // Problems with class constructors
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.<init>"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.*.<init>"),
    ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "scala.*.<init>"),

    // Problem: Missing trait constructor
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*.$init$"),

    // Problem: default parameter
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.*$default$*"), // To check (semantic names vs mangled name?)

    // Problem: Missing Serializable in companions of serializable classes
    ProblemMatcher.make(ProblemKind.MissingParent, "scala.*$"),

    // Problem: Class[T] or ClassTag[T] return type
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
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.package.LinearSeq"),

    // Problem: Incompatible type change is `with` intersection types
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.convert.impl.*.Semi"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.*MapOps.coll"),

    // Problem: Refined type in signature
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.runtime.ScalaRunTime.drop"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.generic.IsMap.Tupled"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsMap.*IsMap"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsSeq.*IsSeq"),

    // Problem: Case class with varargs
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.StringContext.parts"),

    // Problem: Inferred result type of non-private member differs
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.convert.JavaCollectionWrappers.*.iterableFactory"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.convert.JavaCollectionWrappers.*.empty"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.convert.JavaCollectionWrappers.*.mapFactory"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.mutable.LinkedHash*.newBuilder"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.math.Big*.underlying"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.math.Ordering.tryCompare"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.TreeSet.sortedIterableFactory"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.immutable.BitSet.bitSetFactory"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.BitSet.bitSetFactory"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.View.*PartitionMapped.iterator"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessBuilderImpl.*.toSink"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.sys.process.ProcessBuilderImpl.*.toSource"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.concurrent.duration.FiniteDuration.unary_-"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.JavaCollectionWrappers.IteratorWrapper.remove"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.JavaCollectionWrappers.IterableWrapperTrait.iterator"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.util.matching.Regex.MatchIterator.replacementData"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.sys.process.ProcessBuilderImpl.*.createProcess"),

    // Problem: implicit class (method should not be final)
    ProblemMatcher.make(ProblemKind.FinalMember, "scala.collection.convert.*.*"),

    // Problem: implicit class
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterableLowPriority.is*LikeIsIterable"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterableOnce.iterableOnceIsIterableOnce"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterableOnceLowPriority.isIterableLikeIsIterableOnce"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.generic.IsIterable.*OpsIsIterable"),

    // Non-categorized
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.HashTable.init"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.::.next$access$1"),
    ProblemMatcher.make(ProblemKind.MissingTypeMember, "scala.collection.generic.DefaultSerializable._$1"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.*_="),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.math.ScalaNumericConversions.underlying"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.*.superscala$*$*$$*"),
  )
}
