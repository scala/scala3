
trait Ok1 { var i: Int }
class Ok1C extends Ok1 { var i: Int = 1 }

trait Ok2 {
  def i: Int
  def i_=(v: Int): Unit
}
class Ok2C extends Ok2 { override var i: Int = 1 }

// was: variable i of type Int cannot override a mutable variable
trait NotOk {var i: Int}
class NotOkC extends NotOk { override var i: Int = 1 }
