sealed abstract class OrderType(val reverse: OrderType)
case object Buy extends OrderType(Sell)       // error
case object Sell extends OrderType(Buy)
