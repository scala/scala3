import caps.*

class IO extends SharedCapability

// The inferred result type `() ->{io} String` is seen in other compilation units
def mk(io: IO) = () => io.toString
