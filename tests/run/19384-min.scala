@main def Test: Unit =
  f{ x =>
    { (y: Int) => y } match
      case _ => ()
  }

def f(pf: PartialFunction[Unit, Unit]): Unit = ()
