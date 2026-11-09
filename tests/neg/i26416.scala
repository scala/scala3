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

// Version 4: Explicit use declarations to enclosing object: ok
object test4 {
  class Secret(private var s: String) extends caps.SharedCapability:
    def read(): String = s
  var commonsecret: Secret^ = new Secret("Goldfinger")
  class SecretService() uses test4:
    //self: SecretService =>
    val supersecret: Secret^{this} = Secret("Blahblah")
    def leak(): Unit =
      commonsecret = supersecret
}

// Version 5: Explicit use declarations to nested object
package test5 {
  class Secret(private var s: String) extends caps.SharedCapability:
    def read(): String = s

  object Common extends caps.SharedCapability:
    var commonsecret: Secret^ = new Secret("Goldfinger")

  class SecretService() uses Common:
    val supersecret: Secret^{this} = Secret("Blahblah") // error
    def leak(): Unit =
      Common.commonsecret = supersecret
}

// Version 6: Extends SharedCapability
object test6 {
  class Secret(private var s: String):
    def read(): String = s

  private var commonsecret: Secret^ = new Secret("Goldfinger")

  class SecretService extends caps.SharedCapability:
    val supersecret: Secret^{this} = Secret("Blahblah")
    def leak(): Unit =
      commonsecret = supersecret // error
}

// Version 7: Has self type with any
object test7 {

  class Secret(private var s: String):
    def read(): String = s

  private var commonsecret: Secret^ = new Secret("Goldfinger")

  class SecretService { this: SecretService^ =>

    val supersecret: Secret^{this} = Secret("Blahblah")
    def leak(): Unit =
      commonsecret = supersecret // error
  }
}




