class Test0(val valueVal: Test0) extends AnyVal // error: value class cannot wrap itself

class Test1(val x: Int) extends AnyVal
class Test2(val y: Test1) extends AnyVal // error: value class may not wrap another user-defined value class
