trait Container:
    def loopDef: Container
    val loopVal: Container
    def fun(x: Int)(y: Int): Int

def test(c: Container): Int =
    use(c.fun _)
    + use(c.loopDef.fun _)
    + use(c.loopVal.fun _)
    + use(c.loopDef.loopDef.fun _)
    + use(c.loopVal.loopVal.fun _)
    + use(c.loopVal.loopDef.fun _)
    + use(c.loopDef.loopVal.fun _)
    + use(c.loopVal.loopDef.loopVal.fun _)
    + use(c.loopVal.loopDef.loopVal.loopDef.fun _)

def use(f: Int => Int => Int): Int = ???