// An example of how constructor _term_ parameters
// Can be passed to the extends part
// But that doesn't mean the parent type,
// it's just the super constructor call.
class Bar(val y: Long)
class Bar1(val z: Long) extends Bar(z)
