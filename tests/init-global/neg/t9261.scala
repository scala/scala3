sealed abstract class OrderType(val reverse: OrderType)
case object Buy extends OrderType(Sell)       
case object Sell extends OrderType(Buy)

// nopos-error: No warnings can be incurred under -Werror.