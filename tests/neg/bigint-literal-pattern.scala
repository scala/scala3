object Test:
  def bad: Unit =
    (BigInt(1): BigInt) match
      case 1 => ()  // error: .*BigInt
      case _ => () 
