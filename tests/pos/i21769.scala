
infix trait as[From, To]

val conv: (String as Int) = ???
given instance: (String as Int) = ???
def test(ev: (String as Int)) = ???

class F

class K extends (F as K)

class TC1[X]

def doSth[X: TC1 as tc] = ???

class TC2[X]:
  type Self = X

def doSth2[X: {TC1 as tc1, TC2 as tc2}](x: tc2.Self) = ???
