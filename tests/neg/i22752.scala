class Inner(body: Int ?=> Int) extends AnyVal: // error
  def rescue: Int ?=> Int = ???

class Inner2(body: Int => Int) extends AnyVal // ok
