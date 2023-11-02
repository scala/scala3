//> using options -Xfatal-warnings

class Outer:
  type X = { type O = Outer.this.type } // ok
  type Y = { type O = this.type } // warn

// nopos-error: No warnings can be incurred under -Werror.
