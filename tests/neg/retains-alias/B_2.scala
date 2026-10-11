package b

import a.*

// Compiled without capture checking: the `^` on `pf` is dropped, but the
// `Catcher` alias must survive, see the error message in the check file.
object B:
  new A.Catch[Int](1) // error
