trait SAMFunction1[-T1, +R]:
  def apply(x1: T1): R

def fooSAM[T](foo: SAMFunction1[T, T] = (f: T) => f): Unit = ()  // error
