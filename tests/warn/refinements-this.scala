

class Outer:
  type X = { type O = Outer.this.type } // ok
  type Y = { type O = this.type } // warn

