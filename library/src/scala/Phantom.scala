/* Defined synthetically
package scala

trait Phantom {
  /** Phantom.Any does not extend scala.Any */
  protected /*final*/ trait Any

  protected final trait Nothing extends this.Any

  protected final def assume: this.Nothing

  trait Function1[-T1 <: this.Any, +R] {
    def apply(x1: T1): R
  }
  trait ImplicitFunction1[-T1 <: this.Any, +R] extends Function1[T1, R] {
    /*implicit*/ def apply(x1: T1): R
  }
  ...
  trait FunctionN[-T1 <: this.Any, ..., -Tn <: this.Any, +R] {
    def apply(x1: T1, ..., xn: Tn): R
  }
  trait ImplicitFunctionN[-T1 <: this.Any, ..., -Tn <: this.Any, +R] extends FunctionN[T1, ..., Tn, R] {
    /*implicit*/ def apply(x1: T1, ..., xn: Tn): R
  }

}
*/
