class i0 { // error: stack overflow.
  class i1 { type i2 }
  type i3 = i1.i2 // error
  type i4 = i0 { type i1 <: i4 } // this line triggers the overflow
                                 // (and reporting it here would be better).
  // This test ensure the above stack overflow is reported and not hidden by the earlier error.
}
