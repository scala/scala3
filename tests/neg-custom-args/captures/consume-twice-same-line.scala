import language.experimental.captureChecking
import language.experimental.separationChecking
def send(consume x: Object^): Unit = ()
def consumeTwice(consume x: Object^): Unit =
  send(x); send(x)  // error

def consumeThrice(consume x: Object^): Unit =
  send(x); send(x); send(x)  // error // error
