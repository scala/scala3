package dotty.tools.dotc.core

import Types.*, Contexts.*, Symbols.*, Constants.*, Definitions.*,
Denotations.*, Decorators.*, Names.*, StdNames.*, Periods.*

abstract class CompiletimeOpsComparer[N <: Matchable]:
  def moduleClass(using Context): Symbol
  def addType(using Context): Type
  def multiplyType(using Context): Type
  def zero: N
  def one: N
  def minusOne: N
  def add(x: N, y: N): N
  def multiply(x: N, y: N): N
  def isN(x: Any): Boolean
  def toN(x: Any): N

  def equiv(a: Type, b: Type)(using Context) =
    if isSingletonOp(a) && isSingletonOp(b) then
      sumFromTypeNormalizedCached(a) == sumFromTypeNormalizedCached(b)
    else false

  def isSingletonOp(tp: Type)(using Context): Boolean = tp match
    case Op(_, args)                      => args.forall(isSingletonOp)
    case tv: TypeVar if tv.isInstantiated => isSingletonOp(tv.underlying)
    case tp                               => tp.isStable

  object Op:
    def unapply(tp: Type)(using Context): Option[(Name, List[Type])] = tp match
      case AppliedType(tycon: TypeRef, args)
          if tycon.symbol.denot != SymDenotations.NoDenotation && tycon.symbol.owner == moduleClass =>
        Some((tycon.symbol.name, args))
      case _ => None

  val minusOneProd = Product(Nil, minusOne)

  def negate(sum: Sum)(using Context) = Sum(sum.terms.map(_ * minusOneProd))

  val sumFromTypeNormalizedCached =
    cachedTypeOp(tp => sumFromType(tp).normalized)

  def sumFromType(tp: Type)(using Context): Sum =
    underlyingSingletonDeep(tp.dealias) match
      case ConstantType(Constant(c)) if isN(c) =>
        Sum(List(Product(Nil, toN(c))))
      case Op(tpnme.Negate, List(x)) =>
        negate(sumFromType(x))
      case Op(tpnme.Plus, List(x, y)) =>
        sumFromType(x) + sumFromType(y)
      case Op(tpnme.Minus, List(x, y)) =>
        sumFromType(x) + negate(sumFromType(y))
      case Op(tpnme.Times, _) =>
        Sum(List(productFromType(tp)))
      case tp =>
        Sum(List(Product(List(tp))))

  def productFromType(tp: Type)(using Context): Product =
    underlyingSingletonDeep(tp.dealias) match
      case ConstantType(Constant(c)) if isN(c) =>
        Product(Nil, toN(c))
      case Op(tpnme.Times, List(x, y)) =>
        productFromType(x) * productFromType(y)
      case Op(tpnme.Negate | tpnme.Plus | tpnme.Minus, _) =>
        Product(List(sumFromType(tp)))
      case tp =>
        Product(List(tp))

  case class Sum(terms: List[Product] = Nil):
    infix def +(that: Sum) =
      Sum(terms ++ that.terms)
    def normalized(using Context): Sum =
      val normalizedTerms =
        terms
          .map(_.normalized)
          .groupMapReduce(_.facts)(_.c)(add)
          .toList
          .filter({
            case (_, c) if c == zero => false
            case _                   => true
          })
          .map(Product.apply)
          .sortBy(_.hashCode())
      Sum(normalizedTerms)
    def show(using Context): String =
      terms.map(_.show).mkString(" +! ")

  case class Product(facts: List[Sum | Type] = Nil, c: N = one):
    infix def *(that: Product)(using Context) =
      Product(facts ++ that.facts, multiply(c, that.c))
    def normalized(using Context): Product =
      val normalizedFacts = facts
        .map[Sum | Type]({
          case s: Sum   => s.normalized
          case tp: Type => tp
        })
        .sortBy(_.hashCode())
      Product(normalizedFacts, c)
    def show(using Context): String = facts
      .map({
        case p: Sum   => p.show
        case tp: Type => tp.show
      })
      .mkString(" *! ") + " *! " + c

  def underlyingSingletonDeep(tp: Type)(using Context): Type = tp match
    case tp: SingletonType if tp.underlying.isStable =>
      underlyingSingletonDeep(tp.underlying)
    case tv: TypeVar if tv.isInstantiated =>
      underlyingSingletonDeep(tv.underlying)
    case _ => tp

object IntOpsComparer extends CompiletimeOpsComparer[Int]:
  def moduleClass(using Context) = defn.CompiletimeOpsIntModuleClass
  def addType(using Context) = defn.CompiletimeOpsInt_Add
  def multiplyType(using Context) = defn.CompiletimeOpsInt_Multiply
  def zero = 0
  def one = 1
  def minusOne = -1
  def add(x: Int, y: Int) = x + y
  def multiply(x: Int, y: Int) = x * y
  def isN(x: Any) = x.isInstanceOf[Int]
  def toN(x: Any) = x.asInstanceOf[Int]

object LongOpsComparer extends CompiletimeOpsComparer[Long]:
  def moduleClass(using Context) = defn.CompiletimeOpsLongModuleClass
  def addType(using Context) = defn.CompiletimeOpsLong_Add
  def multiplyType(using Context) = defn.CompiletimeOpsLong_Multiply
  def zero = 0L
  def one = 1L
  def minusOne = -1L
  def add(x: Long, y: Long) = x + y
  def multiply(x: Long, y: Long) = x * y
  def isN(x: Any) = x.isInstanceOf[Long]
  def toN(x: Any) = x.asInstanceOf[Long]

def cachedTypeOp[T](f: Type => Context ?=> T): Type => Context ?=> T =
  val cache = collection.mutable.Map.empty[Type, (Period, T)]
  def cached(tp: Type)(using Context) =
    if tp.isProvisional then f(tp)
    else
      val res = cache.updateWith(tp) {
        case Some((p, v)) if p == ctx.period => Some((p, v))
        case Some(_) | None                  => Some((ctx.period, f(tp)))
      }
      res.get._2
  cached
