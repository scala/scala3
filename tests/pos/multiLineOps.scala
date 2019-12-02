val x = 1
  + 2
  + 3

class Channel {
  def ! (msg: String): Channel = this
  def send_! (msg: String): Channel = this
}

val c = Channel()

def send() =
  c ! "hello"
    ! "world"
    send_! "!"

val b: Boolean =
  "hello".isEmpty
  && true &&
  !"hello".isEmpty

val b2: Boolean = {
  println(x)
  !"hello".isEmpty
  ???
}
