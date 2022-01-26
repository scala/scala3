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

object CompiletimeOpsNormalization:
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

    def dropCoefficient(tp: Type): Type = tp match
      case Op(tpnme.Times, List(ConstantType(_), y)) => y
      case _ => tp

    def isFactor(tp: Type): Boolean = tp.dealias match
      case ConstantType(Constant(_))
        | Op(tpnme.Negate, List(_))
        | Op(tpnme.Plus,   List(_, _))
        | Op(tpnme.Minus,  List(_, _))
        | Op(tpnme.Times,  List(_, _)) => false
      case _ => true

    @annotation.tailrec
    def isFactorsNormalForm(tp: Type, max: Type = NoType): Boolean = tp.dealias match
      case Op(tpnme.Times, List(x, y)) =>
        x >= max && isFactor(x)
          && isFactorsNormalForm(y, x)
      case tp =>
        tp >= max
          && isFactor(tp)

    @annotation.tailrec
    def isTermsNormalForm(tp: Type, max: Type = NoType): Boolean = tp.dealias match
      case Op(tpnme.Plus, List(x, y)) =>
        val xFactors = dropCoefficient(x)
        xFactors != max && xFactors >= max
          && isFactorsNormalForm(xFactors)
          && isTermsNormalForm(y, xFactors)
      case _ =>
        val tpFactors = dropCoefficient(tp)
        tpFactors != max && tpFactors >= max
          && isFactorsNormalForm(tpFactors)

    def isNormalForm(tp: Type) = tp match
      case ConstantType(_) => true
      case Op(tpnme.Plus, List(ConstantType(_), y)) => isTermsNormalForm(y)
      case _ => isTermsNormalForm(tp)

    def stableGroupReduce[K, T](seq: Seq[T], key: (T) => K, reduce: (T, T) => T) =
      val m = mutable.LinkedHashMap.empty[K, T]
      for (elem <- seq) do
        val k = key(elem)
        val v = m.get(k) match
            case Some(b) => reduce(b, elem)
            case None    => elem
        m.put(k, v)
      m

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

    case class Sum(prods: List[Product]):
      infix def +(that: Sum) =
        Sum(prods ++ that.prods)
      infix def *(that: Sum) =
        Sum(for p1 <- prods; p2 <- that.prods yield p1 * p2)
      def toType(using Context): Type =
        stableGroupReduce(prods.map(_.sorted), _.facts, _ + _)
          .values
          .map(_.toType)
          .toList
          .sortBy(dropCoefficient)
          .reduceRight((l, r) => AppliedType(ops.addType, List(l, r)))

    def constant(c: N) = Sum(List(Product(Nil, c)))
    val minusOne = constant(ops.minusOne)
    def single(tp: Type) = Sum(List(Product(List(tp), ops.one)))

    def isSingletonOp(tp: Type): Boolean =
      tp.dealias match
        case Op(_, args) => args.forall(isSingletonOp)
        case _: SingletonType => true
        case _ => false

    def convert(tp: Type): Sum = tp.dealias match
      case ConstantType(Constant(c))      => constant(ops.value(c))
      case Op(tpnme.Negate, List(x))      => minusOne * convert(x)
      case Op(tpnme.Plus,   List(x, y))   => convert(x) + convert(y)
      case Op(tpnme.Minus,  List(x, y))   => convert(x) + minusOne * convert(y)
      case Op(tpnme.Times,  List(x, y))   => convert(x) * convert(y)
      case singTp: SingletonType if isSingletonOp(singTp.underlying) =>
        convert(singTp.underlying)
      case _ => tp.tryNormalize match
        case NoType => single(tp)
        case normalized => convert(normalized)

    if !isSingletonOp(tp) || isNormalForm(tp) then
      None
    else
      val result = convert(tp).toType
      assert(isNormalForm(result), f"Not a normal form: ${result.show}")
      Some(result)

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
