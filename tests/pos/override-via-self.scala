// Question: Does TOverrider#f override TCommon#f?
// If not, the accidental override rule applies.
// Dotty used to say no, but with the change to baseClasses in AndTypes says
// yes. Not sure what the right answer is. But in any case we should
// keep the test to notice if there's a difference in behavior.
trait TCommon {
  def f: String
}
class C1 extends TCommon {
  def f = "in C1"
}

trait TOverrider { this: TCommon =>
  override def f = "in TOverrider"   // The overridden self-type member...
}

class C2 extends C1 with TOverrider  // ... failed to override, here. But now it is OK.

