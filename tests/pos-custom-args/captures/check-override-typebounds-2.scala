import language.experimental.captureChecking

class Capbility

trait A:
  type T <: Capbility^{caps.any.rd}

class B extends A:
  type T = C^{caps.any.rd}

class C extends Capbility


trait A2:
  type T[Cap^]

  def takesCap[Cap^](t: T[Cap]): Unit

