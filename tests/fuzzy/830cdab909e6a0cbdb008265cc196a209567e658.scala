class i0 {
class i1 { type i2 }
type i3 = i1.i2
type i4 = i0 { type i1 <: i4 }
type i5 = Int
type i6 = i3.i1 = new i0 { def i4: i5 = ??? }
}