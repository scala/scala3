//> using option -Yno-experimental

import scala.language.experimental.mode
import scala.annotation.experimental

@experimental
def foo = 1

def bar = foo // made `@experimental` by `scala.language.experimental.mode`
