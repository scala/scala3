class E1 extends Exception
class E2 extends Exception

type E1or2 = E1 | E2

def caughtE1orE2(body: => Nothing): Boolean =
  try body
  catch
    case ex: E1or2 => true
    case _ => false

@main def Test =
    assert(caughtE1orE2(throw new E1 {}))
    assert(caughtE1orE2(throw new E2 {}))
    assert(!caughtE1orE2(throw new Exception {}))
