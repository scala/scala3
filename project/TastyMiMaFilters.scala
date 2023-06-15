import java.util.Arrays.asList
import tastymima.intf._

object TastyMiMaFilters {
  val StdlibBootstrapped: java.util.List[ProblemMatcher] = asList(
    // Probably OK
    ProblemMatcher.make(ProblemKind.IncompatibleSelfTypeChange, "scala.*"),

    // Probably OK: Case class with varargs
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.StringContext.parts"), // before: scala.<repeated>[Predef.String]; after: scala.collection.immutable.Seq[Predef.String] @scala.annotation.internal.Repeated

    // Problem: Missing type {scala.runtime.AbstractFunction1}
    ProblemMatcher.make(ProblemKind.MissingParent, "scala.collection.Searching.Found$"),
    ProblemMatcher.make(ProblemKind.MissingParent, "scala.collection.Searching.InsertionPoint$"),
    ProblemMatcher.make(ProblemKind.MissingParent, "scala.collection.StringView$"),
    ProblemMatcher.make(ProblemKind.MissingParent, "scala.jdk.FunctionWrappers.AsJava*$"),
    ProblemMatcher.make(ProblemKind.MissingParent, "scala.jdk.FunctionWrappers.FromJava*$"),
    ProblemMatcher.make(ProblemKind.MissingParent, "scala.ScalaReflectionException$"),
    ProblemMatcher.make(ProblemKind.MissingParent, "scala.UninitializedFieldError$"),

    // Problem: Class[T] or ClassTag[T] with `T` equal to wildcard `_ >: Nothing <: AnyVal` instead of a specific primitive type `T`
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.reflect.ManifestFactory.*.runtimeClass"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.*.elemTag"),

    // Problem: ConstantType for `null` versus `scala.Null`
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.UnrolledBuffer.Unrolled.<init>$default$4"),

    // Problem: Missing type arguments with higher-kinded types
    ProblemMatcher.make(ProblemKind.MissingTypeMember, "scala.collection.SortedSetFactoryDefaults._$5"),
    ProblemMatcher.make(ProblemKind.MissingTypeMember, "scala.collection.SortedMapFactoryDefaults._$6"),

    // Problem? Very complicated signature
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.generic.IsMap.mapOpsIsMap"),

    // Problem: Overriding java method (`public abstract Object underlying();` with `def underlying: Object`)
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.math.Big*.underlying"),
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.math.ScalaNumericConversions.underlying"),

    // Problem: Inferred result type of non-private member differs
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.convert.JavaCollectionWrappers.IterableWrapperTrait.iterator"),
    ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "scala.collection.mutable.UnrolledBuffer.classTagCompanion"),
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.concurrent.FailedNode.string"),

    // Problem: super accessors
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.*.superscala$*$*$$*"), // The member scala.collection.mutable.Cloneable.superscala$collection$mutable$Cloneable$$clone was concrete or did not exist but is abstract in current version

    // Problem: `private[scala] var` in case class
    ProblemMatcher.make(ProblemKind.MissingTermMember, "scala.collection.immutable.::.next$access$1"),

    // Problem Missing setter for `protected var`
    ProblemMatcher.make(ProblemKind.NewAbstractMember, "scala.collection.convert.impl.*_="),

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
