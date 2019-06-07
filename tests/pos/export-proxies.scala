trait Session{
  def call1(): Unit
  def call2(): Unit
  def call3(): Unit
  type T
}

class SessionProxy(val session:Session) extends Session {
  export session.{call2, call3, T}

  def call1(): Unit = {
    println("call1")
    session.call1()
  }
}