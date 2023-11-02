//> using options -Werror

import scala.language.`future-migration`

def foo: Int with String = ??? // warn

// nopos-error: No warnings can be incurred under -Werror.
