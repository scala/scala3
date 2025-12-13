class E1 extends Exception
class E2 extends Exception

type E1or2 = E1 | E2

@main def Test =
    try throw new Exception {}
    catch
      case e: E1or2 => assert(e.isInstanceOf[E1or2])
      case _ => ()
