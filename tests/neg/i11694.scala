def test1 = {
  def f11: (Int => Int) | Unit = x => x + 1
  def f12: Null | (Int => Int) = x => x + 1

  def f21: (Int => Int) | Null = x => x + 1
  def f22: Null | (Int => Int) = x => x + 1

  def f31: (Int => Int) | (Int => Int) = x => x + 1
  def f32: (Int => Int) | (Int => Int) | Unit = x => x + 1

  def f41: (Int => Int) & (Int => Int) = x => x + 1
  def f42: (Int => Int) & (Int => Int) & Any = x => x + 1
}

def test2 = {
  def f1: (Int => String) | (Int => Int) | Null = x => x + 1 // error
  def f2: (Int => String) | Function[String, Int] | Null = x => "" + x // error
  def f3: Function[Int, Int] | Function[String, Int] | Null = x => x + 1 // error
  def f4: (Int => Int) & (Int => Int) & Unit = x => x + 1 // error
}

def test3 = {
  import java.util.function.Function
  val f1: Function[String, Int] | Unit = x => x.length
  val f2: Function[String, Int] | Null = x => x.length
}
