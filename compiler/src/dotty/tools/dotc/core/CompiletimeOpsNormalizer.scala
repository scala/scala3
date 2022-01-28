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
        val res = (if c == 1 && facts.length > 0 then facts else facts :+ ConstantType(Constant(c)))
          .reduceLeft((l, r) => AppliedType(ops.multiplyType, List(l, r)))
        res


    def splitOp(tp: Type, name: Name) = tp match
        case Op(`name`, List(x, y)) => (x, y)
        case _ => (NoType, tp)

    def splitProduct(tp: Type): (N, Type) =
      val (init, last) = splitOp(tp, tpnme.Times)
      last match
        case ConstantType(Constant(c)) if ops.value.isDefinedAt(c) => (ops.value(c), init)
        case _ => (ops.one, tp)

    def dropCoefficient(tp: Type) = splitProduct(tp)._2

    object Product:
      def fromType(tp: Type) =
        def getFacts(tp: Type): List[Type] = tp match
          case Op(tpnme.Times, List(x, y)) => y :: getFacts(x)
          case NoType => Nil
          case _ => List(tp)
        val (c, tail) = splitProduct(tp)
        Product(getFacts(tail), c)

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
          .reduceLeft((l, r) => AppliedType(ops.addType, List(l, r)))

    object Sum:
      def fromType(tp: Type) =
        def getTerms(tp: Type): List[Product] = tp match
          case Op(tpnme.Plus, List(x, y)) => Product.fromType(y) :: getTerms(x)
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
        val xSimp = simp(x)
        val ySimp = simp(y)
        val isNormalForm = ySimp match
          case Op(tpnme.Plus, _) => false
          case _ =>
            val beforeLast = dropCoefficient(splitOp(xSimp, tpnme.Plus)._2)
            val last = dropCoefficient(ySimp)
            beforeLast <= last && (beforeLast != last || (beforeLast.exists && !isSingletonOrSingletonOp(beforeLast)))
        if isNormalForm then None
        else Some((Sum.fromType(xSimp) + Sum.fromType(ySimp)).toType)
      case Op(tpnme.Times,  List(x, y))   =>
        val xSimp = simp(x)
        val ySimp = simp(y)
        val isNormalForm = ySimp match
          case Op(tpnme.Plus | tpnme.Times, _) => false
          case _ => xSimp match
            case Op(tpnme.Plus, _) => false
            case _ =>
              val beforeLast = dropCoefficient(splitOp(xSimp, tpnme.Times)._2)
              val last = dropCoefficient(ySimp)
              dropCoefficient(beforeLast) <= dropCoefficient(last) && beforeLast.exists
        if isNormalForm then None
        else Some((Sum.fromType(xSimp) * Sum.fromType(ySimp)).toType)
    res

  // ----- Ordering --------------------------------------------------------------------

  def TypeOrdering(using Context) = new Ordering[Type]:
    val ListOrdering: Ordering[List[Type]] = scala.math.Ordering.Implicits.seqOrdering(this)
    def compare(a: Type, b: Type): Int = (a, b) match
      case (AppliedType(opA: TypeRef, argsA), AppliedType(opB: TypeRef, argsB)) =>
        val compareOp = NameOrdering.compare(opA.name, opB.name)
        if compareOp != 0 then compareOp else ListOrdering.compare(argsA, argsB)
      case (AppliedType(opA: TypeRef, _), _) => -1
      case (_, AppliedType(opB: TypeRef, _)) => 1

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

      case (NoType, NoType) => 0
      case (NoType, _) => -1
      case (_, NoType) => 1

      case (NoPrefix, NoPrefix) => 0
      case (NoPrefix, _) => -1
      case (_, NoPrefix) => 1

      case (a, b) => 0 // Not comparable
