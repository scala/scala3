import language.experimental.captureChecking
import caps.*


def main() =
  trait Channel[T] extends caps.SharedCapability:
    def send(msg: T): Unit
    def recv(): T

  trait Logger extends caps.SharedCapability:
    def log(msg: String): Unit

  // we can close over anything subsumed by the 'trusted' brand capability, but nothing else
  def runSecure[C^ >: {trusted} <: {trusted}](block: () ->{C} Unit): Unit = block()

  // This is a 'brand" capability to mark what can be mentioned in trusted code
  object trusted extends caps.SharedCapability

  val trustedLogger: Logger^{trusted} = ???
  val trustedChannel: Channel[String]^{trusted} = ???

  val untrustedLogger: Logger^ = ???
  val untrustedChannel: Channel[String]^ = ???

  runSecure: () => // error (???, arose after changing level checking)
    trustedLogger.log("Hello from trusted code") // ok

  runSecure: () => // error (???, arose after changing level checking)
    trustedChannel.send("I can send")
    trustedLogger.log(trustedChannel.recv()) // ok

  runSecure: () => // error (???, arose after changing level checking)
    "I am pure"                             // ok

  runSecure: () => // error
    untrustedLogger.log("I can't be used here")

  runSecure: () => // error
    untrustedChannel.send("I can't be used here")