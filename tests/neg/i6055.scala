opaque type i0           // error: opaque type must have a right-hand side
opaque type i2 <: Int    // error: opaque type must have a right-hand side

opaque type i1[_]        // error: opaque type must have a right-hand side
opaque type x[_] <: Int  // error: opaque type must have a right-hand side
