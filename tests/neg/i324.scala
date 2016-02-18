class O
object O {
   val x: this.type = OO.this // error: OO is not an enclosing class
   val y: O = OO.this // error: OO is not an enclosing class
}
