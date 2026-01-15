import caps.reserve

trait A:
  def foo[@reserve C^]: Object^{C}
  def bar[C^]: Object^{C}

trait B extends A:
  def foo[C^]: Object^{C}  // error
  def bar[@reserve C^]: Object^{C} // error

trait B2:
  def foo[C^]: Object^{C}
  def bar[@reserve C^]: Object^{C}

abstract class C extends A, B2 // error
