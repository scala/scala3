import language.experimental.captureChecking

// Version 1: in package
package test1 {
  class Secret(private var s: String) extends caps.SharedCapability:
    def read(): String = s
  var commonsecret: Secret^ = new Secret("Goldfinger") // error // error
  class SecretService():
    //self: SecretService =>
    val supersecret: Secret^{this} = Secret("Blahblah") // error // error
    def leak(): Unit =
      commonsecret = supersecret
}

// Version 2: in object in empty package
object test2 {
  class Secret(private var s: String) extends caps.SharedCapability:
    def read(): String = s
  var commonsecret: Secret^ = new Secret("Goldfinger") // ok
  class SecretService():
    //self: SecretService =>
    val supersecret: Secret^{this} = Secret("Blahblah") // ok
    def leak(): Unit =
      commonsecret = supersecret
        // ok since this: SecretService^{test2} and test2 contains commonsecret
}

// Version 3: in method
def test3 = {
  class Secret(private var s: String) extends caps.SharedCapability:
    def read(): String = s
  var commonsecret: Secret^ = new Secret("Goldfinger")
  class SecretService():
    //self: SecretService =>
    val supersecret: Secret^{this} = Secret("Blahblah") // error
    def leak(): Unit =
      commonsecret = supersecret
}



