package scala.quoted.compiletime

import scala.annotation.experimental
import scala.quoted.Type

/////// TypeRepr ///////////////////////////////////////////////////////////////

/** A type, type constructors, type bounds or NoPrefix. */
sealed trait TypeRepr {

  /** Shows the type as a String. */
  final def show(using p: Printer[TypeRepr]): String = p.show(this)

  /**
    * Converts this `TypeRepr` to an `Type[?]`
    *
    *  Usage:
    *  ```scala
    *  //{
    *  import scala.quoted.*
    *  def f(using Quotes) = {
    *    val q: Quotes = summon[Quotes]
    *    import q.reflect.*
    *    val typeRepr: TypeRepr = ???
    *  //}
    *    typeRepr.asType match
    *      case '[t] =>
    *        '{ val x: t = ??? }
    *  //{
    *  }
    *  //}
    *  ```
    */
  def asType: Type[?]

  /**
    * Is `self` type the same as `that` type?
    *  This is the case iff `self <:< that` and `that <:< self`.
    */
  def =:=(that: TypeRepr): Boolean

  /** Is this type a subtype of that type? */
  def <:<(that: TypeRepr): Boolean

  /**
    * Widen from singleton type to its underlying non-singleton
    *  base type by applying one or more `underlying` dereferences,
    *  Also go from => T to T.
    *  Identity for all other types. Example:
    *
    *  class Outer { class C ; val x: C }
    *  def o: Outer
    *  <o.x.type>.widen = o.C
    */
  def widen: TypeRepr

  /**
    * Widen from TermRef to its underlying non-termref
    *  base type, while also skipping ByName types.
    */
  def widenTermRefByName: TypeRepr

  /** Widen from ByName type to its result type. */
  def widenByName: TypeRepr

  /** Follow aliases, annotated types until type is no longer alias type, annotated type. */
  def dealias: TypeRepr

  /** Follow non-opaque aliases, annotated types until type is no longer alias type, annotated type. */
  def dealiasKeepOpaques: TypeRepr

  /**
    * A simplified version of this type which is equivalent wrt =:= to this type.
    *  Reduces typerefs, applied match types, and and or types.
    */
  def simplified: TypeRepr

  def classSymbol: Option[Symbol]
  def typeSymbol: Symbol
  def termSymbol: Symbol
  def isSingleton: Boolean

  /**
    * The type of `member` as seen from prefix `self`.
    *
    *  Also see `typeRef` and `termRef`
    */
  def memberType(member: Symbol): TypeRepr

  /** The base classes of this type with the class itself as first element. */
  def baseClasses: List[Symbol]

  /**
    * The least type instance of given class which is a super-type
    *  of this type.  Example:
    *  {{{
    *    class D[T]
    *    class C extends p.D[Int]
    *    ThisType(C).baseType(D) = p.D[Int]
    * }}}
    */
  def baseType(cls: Symbol): TypeRepr

  /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
  def derivesFrom(cls: Symbol): Boolean

  /**
    * Is this type a function type?
    *
    *  @return true if the dealiased type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`
    *
    *  @note The function
    *
    *     - returns true for `given Int => Int` and `erased Int => Int`
    *     - returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.
    */
  def isFunctionType: Boolean

  /**
    * Is this type an context function type?
    *
    *  @see `isFunctionType`
    */
  def isContextFunctionType: Boolean

  /**
    * Is this type a function type with erased parameters?
    *
    *  @see `isFunctionType`
    */
  def isErasedFunctionType: Boolean

  /**
    * Is this type a dependent function type?
    *
    *  @see `isFunctionType`
    */
  def isDependentFunctionType: Boolean

  /**
    * Is this type a `TupleN` type?
    *
    * @return true if the dealiased type of `self` is `TupleN[T1, T2, ..., Tn]`
    */
  def isTupleN: Boolean

  /** The type <this . sym>, reduced if possible. */
  def select(sym: Symbol): TypeRepr

  /** The current type applied to given type arguments: `this[targ]`. */
  def appliedTo(targ: TypeRepr): TypeRepr

  /** The current type applied to given type arguments: `this[targ0, ..., targN]`. */
  def appliedTo(targs: List[TypeRepr]): TypeRepr

  /**
    * Substitutes all types that refer in their symbol attribute to
    *  one of the symbols in `from` by the corresponding types in `to`.
    */
  def substituteTypes(from: List[Symbol], to: List[TypeRepr]): TypeRepr

  /** The applied type arguments (empty if there is no such arguments). */
  def typeArgs: List[TypeRepr]

}
object TypeRepr {

  def quoted(using quotes: Quotes): TypeRepr.Module = quotes.reflectV2.TypeRepr
  given moduleConversion: (quotes: Quotes) => Conversion[TypeRepr.type, TypeRepr.Module] = _ => quotes.reflectV2.TypeRepr

  trait Module private[compiletime] () {

    /** Returns the type or kind (TypeRepr) of T. */
    def of[T <: AnyKind](using Type[T]): TypeRepr

    /** Returns the type constructor of the runtime (erased) class. */
    def typeConstructorOf(clazz: Class[?]): TypeRepr

  }

}

/////// TermRef ///////////////////////////////////////////////////////////////

/** Type of a reference to a type or term symbol. */
sealed trait NamedType extends TypeRepr {
  def qualifier: TypeRepr
  def name: String
}
object NamedType {

  def quoted(using quotes: Quotes): NamedType.Module = quotes.reflectV2.NamedType
  given moduleConversion: (quotes: Quotes) => Conversion[NamedType.type, NamedType.Module] = _ => quotes.reflectV2.NamedType

  trait Module private[compiletime] () {}

}

/////// TermRef ///////////////////////////////////////////////////////////////

/** Type of a reference to a term symbol. */
trait TermRef private[compiletime] () extends NamedType
object TermRef {

  def quoted(using quotes: Quotes): TermRef.Module = quotes.reflectV2.TermRef
  given moduleConversion: (quotes: Quotes) => Conversion[TermRef.type, TermRef.Module] = _ => quotes.reflectV2.TermRef

  def unapply(x: TermRef): (TypeRepr, String) = (x.qualifier, x.name)

  trait Module private[compiletime] () {
    def apply(qual: TypeRepr, name: String): TermRef
    def make(qual: TypeRepr, name: String): TermRef
  }

}

/////// TypeRef ///////////////////////////////////////////////////////////////

/** Type of a reference to a type symbol. */
trait TypeRef private[compiletime] () extends NamedType {
  def isOpaqueAlias: Boolean
  def translucentSuperType: TypeRepr
}
object TypeRef {

  def quoted(using quotes: Quotes): TypeRef.Module = quotes.reflectV2.TypeRef
  given moduleConversion: (quotes: Quotes) => Conversion[TypeRef.type, TypeRef.Module] = _ => quotes.reflectV2.TypeRef

  def unapply(x: TypeRef): (TypeRepr, String) = (x.qualifier, x.name)

  trait Module private[compiletime] () {}

}

/////// ConstantType ///////////////////////////////////////////////////////////////

/** A singleton type representing a known constant value. */
trait ConstantType private[compiletime] () extends TypeRepr {
  def constant: Constant
}
object ConstantType {

  def quoted(using quotes: Quotes): ConstantType.Module = quotes.reflectV2.ConstantType
  given moduleConversion: (quotes: Quotes) => Conversion[ConstantType.type, ConstantType.Module] = _ => quotes.reflectV2.ConstantType

  def unapply(x: ConstantType): Some[Constant] = Some(x.constant)

  trait Module private[compiletime] () {
    def apply(x: Constant): ConstantType
    def make(x: Constant): ConstantType
  }

}

/////// SuperType ///////////////////////////////////////////////////////////////

/** Type of a `super` reference. */
trait SuperType private[compiletime] () extends TypeRepr {
  def thistpe: TypeRepr
  def supertpe: TypeRepr
}
object SuperType {

  def quoted(using quotes: Quotes): SuperType.Module = quotes.reflectV2.SuperType
  given moduleConversion: (quotes: Quotes) => Conversion[SuperType.type, SuperType.Module] = _ => quotes.reflectV2.SuperType

  def unapply(x: SuperType): (TypeRepr, TypeRepr) = (x.thistpe, x.supertpe)

  trait Module private[compiletime] () {
    def apply(thistpe: TypeRepr, supertpe: TypeRepr): SuperType
    def make(thistpe: TypeRepr, supertpe: TypeRepr): SuperType
  }

}

/////// Refinement ///////////////////////////////////////////////////////////////

/** A type with a type refinement `T { type U }`. */
trait Refinement private[compiletime] () extends TypeRepr {
  def parent: TypeRepr
  def name: String
  def info: TypeRepr
}
object Refinement {

  def quoted(using quotes: Quotes): Refinement.Module = quotes.reflectV2.Refinement
  given moduleConversion: (quotes: Quotes) => Conversion[Refinement.type, Refinement.Module] = _ => quotes.reflectV2.Refinement

  def unapply(x: Refinement): (TypeRepr, String, TypeRepr) = (x.parent, x.name, x.info)

  trait Module private[compiletime] () {
    def apply(parent: TypeRepr, name: String, info: TypeRepr): Refinement
    def make(parent: TypeRepr, name: String, info: TypeRepr): Refinement
  }

}

/////// AppliedType ///////////////////////////////////////////////////////////////

/** A higher kinded type applied to some types `T[U]`. */
trait AppliedType private[compiletime] () extends TypeRepr {
  def tycon: TypeRepr
  def args: List[TypeRepr]
}
object AppliedType {

  def quoted(using quotes: Quotes): AppliedType.Module = quotes.reflectV2.AppliedType
  given moduleConversion: (quotes: Quotes) => Conversion[AppliedType.type, AppliedType.Module] = _ => quotes.reflectV2.AppliedType

  def unapply(x: AppliedType): (TypeRepr, List[TypeRepr]) = (x.tycon, x.args)

  trait Module private[compiletime] () {

    /** Applied the type constructor `T` to a list of type arguments `T_1,..,T_n` to create `T[T_1,..,T_n]`. */
    def apply(tycon: TypeRepr, args: List[TypeRepr]): AppliedType

    /** Applied the type constructor `T` to a list of type arguments `T_1,..,T_n` to create `T[T_1,..,T_n]`. */
    def make(tycon: TypeRepr, args: List[TypeRepr]): AppliedType
  }

}

/////// AppliedType ///////////////////////////////////////////////////////////////

/** A type with an annotation `T @foo` */
trait AnnotatedType private[compiletime] () extends TypeRepr {
  def underlying: TypeRepr
  // def annotation: Term // TODO: Term
}
object AnnotatedType {

  def quoted(using quotes: Quotes): AnnotatedType.Module = quotes.reflectV2.AnnotatedType
  given moduleConversion: (quotes: Quotes) => Conversion[AnnotatedType.type, AnnotatedType.Module] = _ => quotes.reflectV2.AnnotatedType

  // def unapply(x: AnnotatedType): (TypeRepr, Term) = (x.underlying, x.annotation) // TODO: Term

  trait Module private[compiletime] () {
    // def apply(underlying: TypeRepr, annot: Term): AnnotatedType // TODO: Term
    // def make(underlying: TypeRepr, annot: Term): AnnotatedType // TODO: Term
  }

}

/////// AppliedType ///////////////////////////////////////////////////////////////

/** Intersection type `T & U` or an union type `T | U`. */
sealed trait AndOrType extends TypeRepr {
  def left: TypeRepr
  def right: TypeRepr
}
object AndOrType {

  def quoted(using quotes: Quotes): AndOrType.Module = quotes.reflectV2.AndOrType
  given moduleConversion: (quotes: Quotes) => Conversion[AndOrType.type, AndOrType.Module] = _ => quotes.reflectV2.AndOrType

  trait Module private[compiletime] () {}

}

/////// AndType ///////////////////////////////////////////////////////////////

/** Intersection type `T & U`. */
trait AndType private[compiletime] () extends AndOrType
object AndType {

  def quoted(using quotes: Quotes): AndType.Module = quotes.reflectV2.AndType
  given moduleConversion: (quotes: Quotes) => Conversion[AndType.type, AndType.Module] = _ => quotes.reflectV2.AndType

  def unapply(x: AndType): (TypeRepr, TypeRepr) = (x.left, x.right)

  trait Module private[compiletime] () {
    def apply(lhs: TypeRepr, rhs: TypeRepr): AndType
    def make(lhs: TypeRepr, rhs: TypeRepr): AndType
  }

}

/////// OrType ///////////////////////////////////////////////////////////////

/** Union type `T | U`. */
trait OrType private[compiletime] () extends AndOrType
object OrType {

  def quoted(using quotes: Quotes): OrType.Module = quotes.reflectV2.OrType
  given moduleConversion: (quotes: Quotes) => Conversion[OrType.type, OrType.Module] = _ => quotes.reflectV2.OrType

  def unapply(x: OrType): (TypeRepr, TypeRepr) = (x.left, x.right)

  trait Module private[compiletime] () {
    def apply(lhs: TypeRepr, rhs: TypeRepr): OrType
    def make(lhs: TypeRepr, rhs: TypeRepr): OrType
  }

}

/////// MatchType ///////////////////////////////////////////////////////////////

/** Type match `T match { case U => ... }`. */
trait MatchType private[compiletime] () extends TypeRepr {
  def bound: TypeRepr
  def scrutinee: TypeRepr
  def cases: List[TypeRepr]
}
object MatchType {

  def quoted(using quotes: Quotes): MatchType.Module = quotes.reflectV2.MatchType
  given moduleConversion: (quotes: Quotes) => Conversion[MatchType.type, MatchType.Module] = _ => quotes.reflectV2.MatchType

  def unapply(x: MatchType): (TypeRepr, TypeRepr, List[TypeRepr]) = (x.bound, x.scrutinee, x.cases)

  trait Module private[compiletime] () {
    def apply(bound: TypeRepr, scrutinee: TypeRepr, cases: List[TypeRepr]): MatchType
    def make(bound: TypeRepr, scrutinee: TypeRepr, cases: List[TypeRepr]): MatchType
  }

}

/////// ByNameType ///////////////////////////////////////////////////////////////

/**
  * Type of a by-name definition of type `=>T`.
  *
  *  May represent by-name parameter such as `thunk` in
  *  ```scala
  *  //{
  *  type T
  *  //}
  *  def log[T](thunk: => T): T = ???
  *  ```
  *
  *  May also represent a the return type of a parameterless method definition such as
  *  ```scala
  *    def foo: Int = ???
  *  ```
  */
trait ByNameType private[compiletime] () extends TypeRepr {
  def underlying: TypeRepr
}
object ByNameType {

  def quoted(using quotes: Quotes): ByNameType.Module = quotes.reflectV2.ByNameType
  given moduleConversion: (quotes: Quotes) => Conversion[ByNameType.type, ByNameType.Module] = _ => quotes.reflectV2.ByNameType

  def unapply(x: ByNameType): Some[TypeRepr] = Some(x.underlying)

  trait Module private[compiletime] () {
    def apply(underlying: TypeRepr): TypeRepr
    def make(underlying: TypeRepr): TypeRepr
  }

}

/////// ParamRef ///////////////////////////////////////////////////////////////

/** Type of a parameter reference. */
trait ParamRef private[compiletime] () extends TypeRepr {
  def binder: TypeRepr
  def paramNum: Int
}
object ParamRef {

  def quoted(using quotes: Quotes): ParamRef.Module = quotes.reflectV2.ParamRef
  given moduleConversion: (quotes: Quotes) => Conversion[ParamRef.type, ParamRef.Module] = _ => quotes.reflectV2.ParamRef

  def unapply(x: ParamRef): (TypeRepr, Int) = (x.binder, x.paramNum)

  trait Module private[compiletime] () {}

}

/////// ThisType ///////////////////////////////////////////////////////////////

/** Type of `this`. */
trait ThisType private[compiletime] () extends TypeRepr {
  def tref: TypeRepr
}
object ThisType {

  def quoted(using quotes: Quotes): ThisType.Module = quotes.reflectV2.ThisType
  given moduleConversion: (quotes: Quotes) => Conversion[ThisType.type, ThisType.Module] = _ => quotes.reflectV2.ThisType

  def unapply(x: ThisType): Some[TypeRepr] = Some(x.tref)

  trait Module private[compiletime] () {}

}

/////// RecursiveThis ///////////////////////////////////////////////////////////////

/** A type that is recursively defined `this`. */
trait RecursiveThis private[compiletime] () extends TypeRepr {
  def binder: RecursiveType
}
object RecursiveThis {

  def quoted(using quotes: Quotes): RecursiveThis.Module = quotes.reflectV2.RecursiveThis
  given moduleConversion: (quotes: Quotes) => Conversion[RecursiveThis.type, RecursiveThis.Module] = _ => quotes.reflectV2.RecursiveThis

  def unapply(x: RecursiveThis): Some[RecursiveType] = Some(x.binder)

  trait Module private[compiletime] () {}

}

/////// RecursiveType ///////////////////////////////////////////////////////////////

/** A type that is recursively defined. */
trait RecursiveType private[compiletime] () extends TypeRepr {
  def underlying: TypeRepr
  def recThis: RecursiveThis
}
object RecursiveType {

  def quoted(using quotes: Quotes): RecursiveType.Module = quotes.reflectV2.RecursiveType
  given moduleConversion: (quotes: Quotes) => Conversion[RecursiveType.type, RecursiveType.Module] = _ => quotes.reflectV2.RecursiveType

  def unapply(x: RecursiveType): Some[TypeRepr] = Some(x.underlying)

  trait Module private[compiletime] () {

    /**
      * Creates a RecType, normalizing its contents. This means:
      *
      *   1. Nested Rec types on the type's spine are merged with the outer one.
      *   2. Any refinement of the form `type T = z.T` on the spine of the type
      *      where `z` refers to the created rec-type is replaced by
      *      `type T`. This avoids infinite recursions later when we
      *      try to follow these references.
      */
    def apply(parentExp: RecursiveType => TypeRepr): RecursiveType

    /** Creates a RecType, normalizing its contents. */
    def make(parentExp: RecursiveType => TypeRepr): RecursiveType

  }

}

/////// LambdaType ///////////////////////////////////////////////////////////////

/** Type of the definition of a method taking a single list of type or term parameters. */
sealed trait LambdaType extends TypeRepr {
  def paramNames: List[String]
  def paramTypes: List[TypeRepr]
  def resType: TypeRepr
}
object LambdaType {

  def quoted(using quotes: Quotes): LambdaType.Module = quotes.reflectV2.LambdaType
  given moduleConversion: (quotes: Quotes) => Conversion[LambdaType.type, LambdaType.Module] = _ => quotes.reflectV2.LambdaType

  trait Module private[compiletime] () {}

}

/////// MethodOrPoly ///////////////////////////////////////////////////////////////

/** Type of the definition of a method taking a single list of type or term parameters. */
sealed trait MethodOrPoly extends LambdaType
object MethodOrPoly {

  def quoted(using quotes: Quotes): MethodOrPoly.Module = quotes.reflectV2.MethodOrPoly
  given moduleConversion: (quotes: Quotes) => Conversion[MethodOrPoly.type, MethodOrPoly.Module] = _ => quotes.reflectV2.MethodOrPoly

  trait Module private[compiletime] () {}

}

/////// MethodType ///////////////////////////////////////////////////////////////

/** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
trait MethodType private[compiletime] () extends MethodOrPoly {

  /** Is this the type of parameter clause like `(implicit X1, ..., Xn)`, `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)`. */
  def isImplicit: Boolean

  /** Is this the type of parameter clause like `(using X1, ..., Xn)` or `(using x1: X1, x2: X2, ... )`. */
  def isContextual: Boolean

  /** Returns a MethodTypeKind object representing the implicitness of the MethodType parameter clause. */
  def methodTypeKind: MethodTypeKind

  /** List of `erased` flags for each parameters of the clause. */
  @experimental
  def erasedParams: List[Boolean]

  /** Whether the clause has any erased parameters. */
  @experimental
  def hasErasedParams: Boolean
  def param(idx: Int): TypeRepr
}
object MethodType {

  def quoted(using quotes: Quotes): MethodType.Module = quotes.reflectV2.MethodType
  given moduleConversion: (quotes: Quotes) => Conversion[MethodType.type, MethodType.Module] = _ => quotes.reflectV2.MethodType

  def unapply(x: MethodType): (List[String], List[TypeRepr], TypeRepr) = (x.paramNames, x.paramTypes, x.resType)

  trait Module private[compiletime] () {
    def apply(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType
    def make(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType
    def apply(kind: MethodTypeKind)(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType
    def make(kind: MethodTypeKind)(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType
  }

}

///////  ///////////////////////////////////////////////////////////////

/** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
trait PolyType private[compiletime] () extends MethodOrPoly {
  def param(idx: Int): TypeRepr
  def paramBounds: List[TypeBounds]
}
object PolyType {

  def quoted(using quotes: Quotes): PolyType.Module = quotes.reflectV2.PolyType
  given moduleConversion: (quotes: Quotes) => Conversion[PolyType.type, PolyType.Module] = _ => quotes.reflectV2.PolyType

  def unapply(x: PolyType): (List[String], List[TypeBounds], TypeRepr) = (x.paramNames, x.paramBounds, x.resType)

  trait Module private[compiletime] () {
    def apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => TypeRepr): PolyType
    def make(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => TypeRepr): PolyType
  }

}

/////// TypeLambda ///////////////////////////////////////////////////////////////

/** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
trait TypeLambda private[compiletime] () extends LambdaType {

  /** Reference to the i-th parameter. */
  def param(idx: Int): TypeRepr

  /** Type bounds of the i-th parameter. */
  def paramBounds: List[TypeBounds]

  /**
    * Variance flags for the i-th parameter
    *
    *  Variance flags can be one of `Flags.{Covariant, Contravariant, EmptyFlags}`.
    */
  def paramVariances: List[Flags]
}
object TypeLambda {

  def quoted(using quotes: Quotes): TypeLambda.Module = quotes.reflectV2.TypeLambda
  given moduleConversion: (quotes: Quotes) => Conversion[TypeLambda.type, TypeLambda.Module] = _ => quotes.reflectV2.TypeLambda

  def unapply(x: TypeLambda): (List[String], List[TypeBounds], TypeRepr) = (x.paramNames, x.paramBounds, x.resType)

  trait Module private[compiletime] () {
    def apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr): TypeLambda
    def make(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr): TypeLambda
  }

}

/////// MatchCase ///////////////////////////////////////////////////////////////

/**
  * Case of a `MatchType` containing pattern `case P => R`.
  *
  *  Note: cases with type bindings are represented nested in a `TypeLambda`.
  */
trait MatchCase private[compiletime] () extends TypeRepr {

  /** Pattern `P` of `case P => R` in a `MatchType`. */
  def pattern: TypeRepr

  /** RHS `R` of `case P => R` in a `MatchType`. */
  def rhs: TypeRepr
}
object MatchCase {

  def quoted(using quotes: Quotes): MatchCase.Module = quotes.reflectV2.MatchCase
  given moduleConversion: (quotes: Quotes) => Conversion[MatchCase.type, MatchCase.Module] = _ => quotes.reflectV2.MatchCase

  def unapply(x: MatchCase): (TypeRepr, TypeRepr) = (x.pattern, x.rhs)

  trait Module private[compiletime] () {
    /* Create match type case `case <pattern> => <rhs>` */
    def apply(pattern: TypeRepr, rhs: TypeRepr): MatchCase
    /* Create match type case `case <pattern> => <rhs>` */
    def make(pattern: TypeRepr, rhs: TypeRepr): MatchCase
  }

}

/////// TypeBounds ///////////////////////////////////////////////////////////////

/** Type bounds. */
trait TypeBounds private[compiletime] () extends TypeRepr {
  def low: TypeRepr
  def hi: TypeRepr
}
object TypeBounds {

  def quoted(using quotes: Quotes): TypeBounds.Module = quotes.reflectV2.TypeBounds
  given moduleConversion: (quotes: Quotes) => Conversion[TypeBounds.type, TypeBounds.Module] = _ => quotes.reflectV2.TypeBounds

  def unapply(x: TypeBounds): (TypeRepr, TypeRepr) = (x.low, x.hi)

  trait Module private[compiletime] () {
    def apply(low: TypeRepr, hi: TypeRepr): TypeBounds
    def make(low: TypeRepr, hi: TypeRepr): TypeBounds
    def empty: TypeBounds
    def upper(hi: TypeRepr): TypeBounds
    def lower(lo: TypeRepr): TypeBounds
  }

}

/////// NoPrefix ///////////////////////////////////////////////////////////////

/** NoPrefix for a type selection. */
trait NoPrefix private[compiletime] () extends TypeRepr
object NoPrefix {

  def quoted(using quotes: Quotes): NoPrefix.Module = quotes.reflectV2.NoPrefix
  given moduleConversion: (quotes: Quotes) => Conversion[NoPrefix.type, NoPrefix.Module] = _ => quotes.reflectV2.NoPrefix

  def unapply(x: NoPrefix): true = true

  trait Module private[compiletime] () {}

}

/////// FlexibleType ///////////////////////////////////////////////////////////////

/** Flexible types for explicit nulls. */
trait FlexibleType private[compiletime] () extends TypeRepr {
  def underlying: TypeRepr
  def lo: TypeRepr
  def hi: TypeRepr
}
object FlexibleType {

  def quoted(using quotes: Quotes): FlexibleType.Module = quotes.reflectV2.FlexibleType
  given moduleConversion: (quotes: Quotes) => Conversion[FlexibleType.type, FlexibleType.Module] = _ => quotes.reflectV2.FlexibleType

  def unapply(x: FlexibleType): Option[TypeRepr] = Some(x.underlying)

  trait Module private[compiletime] () {
    def apply(tp: TypeRepr): FlexibleType
    def make(tp: TypeRepr): FlexibleType
  }

}
