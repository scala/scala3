object Test {
def foo22[T](c1:T, c2:T, c3:T, c4:T, c5:T, c6:T, c7:T, c8:T, c9:T, c10:T, c11:T, c12:T, c13:T, c14:T, c15:T, c16:T, c17:T, c18:T, c19:T, c20:T, c21:T, c22:T): Int => T = {
    (a: Int) => { // This lambda ends with 23 parameters (c1 to c22 and a)
      c22; c21; c20; c19; c18; c17; c16; c15; c14; c13; c12; c11; c10; c9; c8; c7; c6; c5; c4; c3; c2; c1
    }
  }
}
