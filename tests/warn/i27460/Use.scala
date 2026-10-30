//> using options -release 17

// The macro call suspends this unit to a second run, which reads the annotation again
def lock2(i: Integer) = i.synchronized { id(1) } // warn
