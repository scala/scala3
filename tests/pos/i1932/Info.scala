class Info(k: Keyboard)

sealed trait Thing
class Keyboard            extends Thing
class Laptop(k: Keyboard) extends Thing
