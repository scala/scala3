object Test {
  implicit def i1[I2, I3](i4: I2)(implicit i5: I2 => I3): I3 = ( ) // error
}
