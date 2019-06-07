opaque type i0           // error: opaque type must be an alias type
opaque type i2 <: Int    // error: opaque type must be an alias type

opaque type i1[_]        // error: opaque type must be an alias type
opaque type x[_] <: Int  // error: opaque type must be an alias type
