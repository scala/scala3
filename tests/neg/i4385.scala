class i0 {
  class i1 { type i2 }
  type i3 = i1.i2 // error
  type i4 = i0 { type i1 <: i4 } // error: recursion limit exceeded
  // This test ensures the above stack overflow is reported and not hidden by the earlier error.
}
