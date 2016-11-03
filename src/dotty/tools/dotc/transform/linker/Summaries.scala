package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags.FlagSet
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.{Hashable, TypeErasure}
import dotty.tools.dotc.transform.linker.CollectSummaries.SubstituteByParentMap

import collection.mutable

object Summaries {
  val version: Int = 1
  val sectionName = "$summaries"


   class ClosureType(val meth: tpd.Closure, val u: Type, val implementedMethod: Symbol, val outerTargs: OuterTargs)(implicit ctx: Context) extends SingletonType {
     /** The type to which this proxy forwards operations. */
     def underlying(implicit ctx: Context): Type = u

     /** customized hash code of this type.
       * NotCached for uncached types. Cached types
       * compute hash and use it as the type's hashCode.
       */
     override val hash: Int = implementedMethod.hashCode() + meth.meth.symbol.hashCode()

     override def hashCode() = hash

     override def equals(other: Any): Boolean = other match {
       case that: ClosureType =>
           meth == that.meth &&
           u == that.u &&
           implementedMethod == that.implementedMethod
       case _ => false
     }

     override def toString: String = s"ClosureType($meth, $u, $implementedMethod, $outerTargs)"
   }

   class PreciseType(private[PreciseType] val u: Type) extends SingletonType {

     /** customized hash code of this type.
       * NotCached for uncached types. Cached types
       * compute hash and use it as the type's hashCode.
       */
     def hash: Int = {
       val underlying = u.hash
       if (underlying == Hashable.NotCached) Hashable.NotCached
       else if (underlying == Hashable.NotCached - 1) underlying
       else underlying + 1
     }

     override def hashCode(): Int = hash

     def underlying(implicit ctx: Context): Type = u

     override def equals(other: Any): Boolean = other match {
       case that: PreciseType => this.u == that.u
       case _ => false
     }

     override def toString: String = s"PreciseType($u)"
   }

  class JavaAllocatedType(private[JavaAllocatedType] val u: Type) extends SingletonType {

    /** customized hash code of this type.
      * NotCached for uncached types. Cached types
      * compute hash and use it as the type's hashCode.
      */
    def hash: Int = {
      val underlying = u.hash
      if (underlying == Hashable.NotCached) Hashable.NotCached
      else if (underlying == Hashable.NotCached - 1) underlying
      else underlying + 1
    }

    override def hashCode(): Int = hash

    def underlying(implicit ctx: Context): Type = u

    override def equals(other: Any): Boolean = other match {
      case that: JavaAllocatedType => this.u == that.u
      case _ => false
    }

    override def toString: String = s"JavaAllocatedType($u)"
  }

   class ErazedType extends UncachedProxyType {
     /** The type to which this proxy forwards operations. */
     def underlying(implicit ctx: Context): Type = ctx.definitions.AnyType
   }


  case class CallInfo(call: Type, // this is type of method, that includes full type of reciever, eg: TermRef(reciever, Method)
                       targs: List[Type],
                       argumentsPassed: List[Type],
                       source: CallInfo = null // When source is not null this call was generated as part of a call to source
                       )(implicit ctx: Context) {
    call.widenDealias match {
      case t: PolyType => assert(t.paramNames.size == targs.size)
      case _ =>
    }
  }

  final case class OuterTargs(mp: Map[Symbol, Map[Name, Type]]) extends AnyVal {
    def ++(parent: (Symbol, List[Type]))(implicit ctx: Context): OuterTargs = {
      parent._2.foldLeft(this)((x, y) => x.add(parent._1, y))
    }
    def +(parent: (Symbol, Type))(implicit ctx: Context): OuterTargs = {
      this.add(parent._1, parent._2)
    }
    def add(parent: Symbol, tp: Type)(implicit ctx: Context): OuterTargs = {
      this.add(parent, tp.typeSymbol.name, tp)
    }
    def add(parent: Symbol, name: Name, tp: Type): OuterTargs = {
      val old = mp.getOrElse(parent, Map.empty)
      new OuterTargs(mp.updated(parent, old + (name -> tp)))
    }
    def nonEmpty = mp.nonEmpty
    def ++(other: OuterTargs)(implicit ctx: Context) = {
      other.mp.foldLeft(this) { (x, y) =>
        y._2.foldLeft(x: OuterTargs)((x: OuterTargs, z: (Name, Type)) => x.add(y._1, z._1, z._2))
      }
    }
    def combine(environment: OuterTargs)(implicit ctx: Context): OuterTargs = {
      val subst = new SubstituteByParentMap(environment)
      val newMap = mp.map(x => (x._1, x._2.map(x => (x._1, subst.apply(x._2)))))
      OuterTargs(newMap)
    }
  }

  object OuterTargs {
    def empty = new OuterTargs(Map.empty)
  }

  private var nextCallId = 0

  class CallWithContext(call: Type, targs: List[Type], argumentsPassed: List[Type], val outerTargs: OuterTargs,
                        val parent: CallWithContext, val callee: CallInfo, val isEntryPoint: Boolean = false)(implicit ctx: Context) extends CallInfo(call, targs, argumentsPassed) {

    val id = { nextCallId += 1; nextCallId }

    val outEdges = mutable.HashMap[CallInfo, List[CallWithContext]]().withDefault(x => Nil)

    override def hashCode(): Int = super.hashCode() ^ outerTargs.hashCode()

    override def equals(obj: Any): Boolean = {
      obj match {
        case t: CallWithContext => t.call == this.call && t.targs == this.targs && this.argumentsPassed == t.argumentsPassed &&  this.outerTargs == t.outerTargs && this.source == t.source
        case _ => false
      }
    }

    override def toString: String = s"CallWithContext($call, $targs, $argumentsPassed, $outerTargs, $parent, $callee)"
  }

  class TypeWithContext(val tp: Type, val outerTargs: OuterTargs) {
    val castsCache: mutable.Set[Cast] = mutable.Set.empty

    override def hashCode(): Int = tp.hashCode() * 31 + outerTargs.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case t: TypeWithContext => t.tp.equals(tp) && (t.outerTargs equals outerTargs)
      case _ => false
    }

    override def toString: String = s"TypeWithContext($tp, $outerTargs)"
  }

  case class Cast(from: Type, to: Type)(implicit ctx: Context) {
    override def equals(other: Any): Boolean = {
      other match {
        case Cast(a, b) =>
          a =:= from && b =:= to
        case _ => false
      }
    }

    override def hashCode(): Int =
      from.typeSymbol.hashCode() * 31 + to.typeSymbol.hashCode()

    override def toString: String = s"Cast($from, $to)"
  }


  case class MethodSummary(methodDef: Symbol,
                           var thisAccessed: Boolean,
                           methodsCalled: mutable.Map[Type, List[CallInfo]],
                           // allocatedLambdas
                           var accessedModules: List[Symbol],
                           argumentReturned: Byte, // -1 if not known
                           var argumentStoredToHeap: List[Boolean] // not currently collected
                          )

  def simplifiedClassOf(t: Type)(implicit ctx: Context) = {
    val s = t.widenDealias.finalResultType.classSymbol.orElse(TypeErasure.erasure(t.widenDealias.finalResultType).classSymbol)
    assert(s.exists)
    s
  }
}
