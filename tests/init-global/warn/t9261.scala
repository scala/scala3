sealed abstract class OrderType(val reverse: OrderType)
case object Buy extends OrderType(Sell) // warn
case object Sell extends OrderType(Buy) // warn
