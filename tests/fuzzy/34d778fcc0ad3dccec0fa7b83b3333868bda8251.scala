abstract class i0 {
type i1
type i2 <: i1
type i3 <: i4 {
type i2 = i3
val i5 = new i3
val i6 = new i2
val i7 = new i3
val i6 = i5 _
}