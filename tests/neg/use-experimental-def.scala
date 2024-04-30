

import scala.annotation.experimental

@experimental def foo: Int = 1

def bar: Int = foo // error
