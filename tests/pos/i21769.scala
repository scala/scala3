
infix trait as[From, To]

val conv: (String as Int) = ???
given instance: (String as Int) = ???
def test(ev: (String as Int)) = ???

class F

class K extends (F as K)
