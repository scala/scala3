import Effect.isPure
object effects extends App {

  def f1(implicit x: CanThrow[NullPointerException]) = 3
  def f2(x: Int)(implicit y: CanThrow[NullPointerException]) = x

  def g(implicit eff: CanThrow[NullPointerException]) = {
    assert(f1 == 3)
    assert(f2(3) == 3)
  }

  // An implicit function over effects - something like this will be be turned into syntax
  //    implicit A => B
  trait EffFun[-A <: Pure, B] {
    def apply(implicit eff: A): B
  }

  type throws[T, E <: Throwable] = EffFun[CanThrow[E], T]

  def Try[T, E <: Throwable](body: T throws E)(handler: E => T): T = {
    // Use unsafe effect import to build a safe masking operation `Try`
    implicit val eff: CanThrow[E] = Effect.canThrow
    try body.apply
    catch {
      case ex: E => handler(ex)
    }
  }

  def Throw[E <: Throwable : CanThrow](ex: E): Nothing = throw ex

  class Ex extends Exception

  Try {
    new EffFun[CanThrow[Ex], Unit] {
      def apply(implicit eff: CanThrow[Ex]) = Throw(new Ex)
    }
  } {
    ex => println("caught")
  }

  import java.util.{NoSuchElementException => NSE}

  def unwrap[T](x: Option[T]): T throws NSE =
    new (T throws NSE) {
      def apply(implicit eff: CanThrow[NSE]) =
        x.get
    }

  def getOrElse[T](xo: Option[T], default: T): T =
    Try {
      new EffFun[CanThrow[NSE], T] {
        def apply(implicit eff: CanThrow[NSE]) =
          unwrap(xo).apply
      }
    } {
      (ex: NSE) => default
    }

  def summon[E <: Pure](implicit eff: E): Unit = ()

  type EFF <: Pure

  { import Effect.{canThrow, canBoth}
    implicit val eff: EFF = Effect.isImpure
    summon[CanThrow[AssertionError] eff_& CanThrow[NullPointerException]]
    summon[EFF]
    summon[CanThrow[AssertionError] eff_& EFF]
  }
}
