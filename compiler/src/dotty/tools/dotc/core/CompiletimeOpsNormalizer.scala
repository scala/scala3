package dotty.tools
package dotc
package core

import collection.mutable
import Constants._
import Contexts._
import Definitions._
import Denotations._
import Names._
import StdNames._
import Symbols._
import SymDenotations.NoDenotation
import Types._

object CompiletimeOpsNormalizer:
  trait Normalizable[T]:
    def moduleClass(using Context): Symbol
    def addType(using Context): Type
    def multiplyType(using Context): Type
    val zero: T
    val one: T
    val minusOne: T
    val add: (T, T) => T
    val multiply: (T, T) => T
    val value: PartialFunction[Any, T]

  given Normalizable[Long] with
    def moduleClass(using Context) = defn.CompiletimeOpsLongModuleClass
    def addType(using Context) = defn.CompiletimeOpsLong_Add
    def multiplyType(using Context) = defn.CompiletimeOpsLong_Multiply
    val zero = 0L
    val one = 1L
    val minusOne = -1L
    val add = _ + _
    val multiply = _ * _
    val value = {
      case n: Long => n
      case n: Int => n
      case n: Short => n
      case n: Char => n
    }

  given Normalizable[Int] with
    def moduleClass(using Context) = defn.CompiletimeOpsIntModuleClass
    def addType(using Context) = defn.CompiletimeOpsInt_Add
    def multiplyType(using Context) = defn.CompiletimeOpsInt_Multiply
    val zero = 0
    val one = 1
    val minusOne = -1
    val add = _ + _
    val multiply = _ * _
    val value = {
      case n: Int => n
      case n: Short => n
      case n: Char => n
    }

  def linearNormalForm[@specialized(Int, Long) N](tp: Type)(using Context)(using ops: Normalizable[N]) =
    import scala.math.Ordering.Implicits.seqOrdering
    import scala.math.Ordered.orderingToOrdered
    given Ordering[Type] = TypeOrdering

    object Op:
        def unapply(tp: Type): Option[(Name, List[Type])] = tp match
          case AppliedType(tycon: TypeRef, args)
            if tycon.symbol.denot != NoDenotation
                && tycon.symbol.owner == ops.moduleClass =>
              Some((tycon.symbol.name, args))
          case _ => None

    def isSingletonOrSingletonOp(tp: Type): Boolean = tp match
      case Op(_, args) => args.forall(isSingletonOrSingletonOp)
      case _: SingletonType => true
      case _ => false

    def underlyingSingletonDeep(tp: Type)(using Context): Type = tp match
      case tp: SingletonType if isSingletonOrSingletonOp(tp.underlying) => tp.underlying
      case _ => tp

    def simp(tp: Type) = underlyingSingletonDeep(tp.normalized.dealias)

    case class Product(facts: List[Type], c: N):
      infix def +(that: Product) =
        assert(facts.sorted == that.facts.sorted)
        Product(facts, ops.add(c, that.c))
      infix def *(that: Product) =
        Product(facts ++ that.facts, ops.multiply(c, that.c))
      def sorted: Product =
        Product(facts.sorted, c)
      def toType(using Context): Type =
        (if c == 1 && facts.length > 0 then facts else ConstantType(Constant(c)) :: facts)
          .reduceRight((l, r) => AppliedType(ops.multiplyType, List(l, r)))

    object Product:
      def fromType(tp: Type) =
        def getFacts(tp: Type): List[Type] = tp match
          case Op(tpnme.Times, List(x, y)) => x :: getFacts(y)
          case _ => List(tp)
        tp match
          case Op(tpnme.Times, List(ConstantType(Constant(c)), y)) if ops.value.isDefinedAt(c) =>
            Product(getFacts(y), ops.value(c))
          case ConstantType(Constant(c)) if ops.value.isDefinedAt(c) =>
            Product(Nil, ops.value(c))
          case _ =>
            Product(List(tp), ops.one)

    def dropCoefficient(tp: Type): Type = tp match
      case Op(tpnme.Times, List(ConstantType(_), y)) => y
      case ConstantType(_) => NoType
      case _ => tp

    case class Sum(terms: List[Product]):
      infix def +(that: Sum) =
        Sum(terms ++ that.terms)
      infix def *(that: Sum) =
        Sum(for p1 <- terms; p2 <- that.terms yield p1 * p2)
      def toType(using Context): Type =
        val groupedSingletonProds = mutable.LinkedHashMap.empty[List[Type], Product]
        val nonSingletonProds = mutable.ArrayBuffer.empty[Product]
        for(prod <- terms.map(_.sorted)) do
          if prod.facts.forall(isSingletonOrSingletonOp) then
            groupedSingletonProds.updateWith(prod.facts)({
              case Some(prev) => Some(prev + prod)
              case None       => Some(prod)
            })
          else
            nonSingletonProds.addOne(prod)

        groupedSingletonProds
          .values
          .concat(nonSingletonProds)
          .map(_.toType)
          .toList
          .sortBy(dropCoefficient)
          .reduceRight((l, r) => AppliedType(ops.addType, List(l, r)))

    object Sum:
      def fromType(tp: Type) =
        def getTerms(tp: Type): List[Product] = tp match
          case Op(tpnme.Plus, List(x, y)) => Product.fromType(x) :: getTerms(y)
          case _ => List(Product.fromType(tp))
        Sum(getTerms(tp))

    val minusOne = Sum(List(Product(Nil, ops.minusOne)))
    def single(tp: Type) = Sum(List(Product(List(tp), ops.one)))

    val res = tp match
      case Op(tpnme.Negate, List(x))      =>
        Some((minusOne * Sum.fromType(simp(x))).toType)
      case Op(tpnme.Minus,  List(x, y))   =>
        Some((Sum.fromType(simp(x)) + minusOne *  Sum.fromType(simp(y))).toType)
      case Op(tpnme.Plus,   List(x, y))   =>
        val xNormalized = simp(x)
        val yNormalized = simp(y)
        val isNormalForm = xNormalized match
          case Op(tpnme.Plus, _) => false
          case _ =>
            val fact1 = dropCoefficient(xNormalized)
            val fact2 = dropCoefficient(
              yNormalized match
                case Op(tpnme.Plus, List(yX, _)) => yX
                case _ => yNormalized
            )
            fact1 <= fact2 && (fact1 != fact2 || (fact2.exists && !isSingletonOrSingletonOp(fact2)))
        if isNormalForm then None
        else Some((Sum.fromType(xNormalized) +  Sum.fromType(yNormalized)).toType)
      case Op(tpnme.Times,  List(x, y))   =>
        val xNormalized = simp(x)
        val yNormalized = simp(y)
        val isNormalForm = xNormalized match
          case Op(tpnme.Plus | tpnme.Times, _) => false
          case _ => yNormalized match
            case Op(tpnme.Plus, _) => false
            case _ => dropCoefficient(xNormalized) <= dropCoefficient(yNormalized)
        if isNormalForm then None
        else Some((Sum.fromType(xNormalized) * Sum.fromType(yNormalized)).toType)
    res

  // ----- Ordering --------------------------------------------------------------------

  def TypeOrdering(using Context) = new Ordering[Type]:
    val ListOrdering: Ordering[List[Type]] = scala.math.Ordering.Implicits.seqOrdering(this)
    def compare(a: Type, b: Type): Int = (a, b) match
      case (NoType, NoType) => 0
      case (NoType, _) => -1
      case (_, NoType) => 1

      case (NoPrefix, NoPrefix) => 0
      case (NoPrefix, _) => -1
      case (_, NoPrefix) => 1

      case (ConstantType(Constant(valueA: Short)), ConstantType(Constant(valueB: Short))) =>
        valueA compare valueB
      case (ConstantType(Constant(_: Short)), _) => -1
      case (_, ConstantType(Constant(_: Short))) => 1

      case (ConstantType(Constant(valueA: Int)), ConstantType(Constant(valueB: Int))) =>
        valueA compare valueB
      case (ConstantType(Constant(_: Int)), _) => -1
      case (_, ConstantType(Constant(_: Int))) => 1

      case (ConstantType(Constant(valueA: Long)), ConstantType(Constant(valueB: Long))) =>
        valueA compare valueB
      case (ConstantType(Constant(_: Long)), _) => -1
      case (_, ConstantType(Constant(_: Long))) => 1

      case (ConstantType(Constant(valueA: Float)), ConstantType(Constant(valueB: Float))) =>
        valueA compare valueB
      case (ConstantType(Constant(_: Float)), _) => -1
      case (_, ConstantType(Constant(_: Float))) => 1

      case (ConstantType(Constant(valueA: Double)), ConstantType(Constant(valueB: Double))) =>
        valueA compare valueB
      case (ConstantType(Constant(_: Double)), _) => -1
      case (_, ConstantType(Constant(_: Double))) => 1

      case (ConstantType(Constant(valueA: String)), ConstantType(Constant(valueB: String))) =>
        valueA compare valueB
      case (ConstantType(Constant(_: String)), _) => -1
      case (_, ConstantType(Constant(_: String))) => 1

      case (ThisType(typeRefA), ThisType(typeRefB)) => compare(typeRefA, typeRefB)
      case (_: ThisType, _) => -1
      case (_, _: ThisType) => 1

      case (a: NamedType, b: NamedType) =>
        if a.isTerm && b.isType then -1
        else if b.isType && a.isTerm then 1
        else
          val comparePrefix = compare(a.prefix, b.prefix)
          if comparePrefix != 0 then comparePrefix else NameOrdering.compare(a.name, b.name)
      case (_: NamedType, _) => -1
      case (_, _: NamedType) => 1

      case (AppliedType(opA: TypeRef, argsA), AppliedType(opB: TypeRef, argsB)) =>
        val compareOp = NameOrdering.compare(opA.name, opB.name)
        if compareOp != 0 then compareOp else ListOrdering.compare(argsA, argsB)
      case (AppliedType(opA: TypeRef, _), _) => -1
      case (_, AppliedType(opB: TypeRef, _)) => 1

      case (a, b) => 0 // Not comparable
