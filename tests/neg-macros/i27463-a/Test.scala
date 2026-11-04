package object macros:

  class Derived(private[macros] val any: Any) extends Macros:
    def test1(d: Derived): Unit = super.test(d)

// nopos-error
