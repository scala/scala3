class Dog:
  inline given bark(using msg: String = "Woof!"): String = s"bark: $msg"

class Wolf:
  private val dog: Dog = Dog()
  export dog.given

def test =
  val w = Wolf()
  import w.given
  summon[String]
