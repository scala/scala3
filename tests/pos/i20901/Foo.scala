//> using options -Ytest-pickler-check

import reflect.ClassTag

class Foo:
  def mkArray[T: ClassTag] = ???
