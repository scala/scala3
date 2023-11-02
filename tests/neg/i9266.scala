//> using options -Xfatal-warnings

import language.`future-migration`

def test = { implicit x: Int => x + x } // warn

// nopos-error: No warnings can be incurred under -Werror.
