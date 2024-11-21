import caps.*

trait AbstractWrong:
    type C <: CapSet
    def boom(): Unit^{C^} // error

trait Abstract:
    type C <: CapSet^
    def boom(): Unit^{C^}

class Concrete extends Abstract:
    type C = Nothing
    def boom() = () // error

