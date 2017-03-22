
class PhantomFun1NoApply extends Function1[Boo.Casper, Unit] // error: class PhantomFun1NoApply needs to be abstract, since def apply: (p0: Casper)Unit is not defined

object Boo extends Phantom {
  type Casper <: this.Any
}
