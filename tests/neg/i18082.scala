object O { val a = 1; val b = 2 }

import O.{a as b, a} // OK
import O.{a as b, b as a} // OK
import O.{a as b, b as b} // error
import O.{a as b, b} // error
import O.{a, a} // error

import O.{a as _, b as _} // ok
import O.{a as _, a as _} // error
import O.{a, a as _} // error

