package mycode

class MyServerCall extends mylib.PublicServerCallBase {
  override def close(): Unit = {
    println("MyServerCall.close")
    super.close()
  }
}
