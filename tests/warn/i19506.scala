//> using options -source 3.4

trait Reader[T]
def read[T: Reader](s: String, trace: Boolean = false)(implicit i: Int = 42): T = ???

// if apply kind is not using when retrying to insert the default arg, it will warn:
// Context bounds will map to context parameters.
// A `using` clause is needed to pass explicit arguments to them.
// The method to invoke (read) might have been compiled under 3.3,
// or (as above) the contextual parameter may have been merged with an existing implicit param list.
def Test =
  implicit def reader: Reader[Object] = new Reader[Object]{}
  read[Object]("") // no warn
