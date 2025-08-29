// Test cases to ensure valid overrides still work after fixing #22310

abstract class ValidCase1[T] {
  def method(arg: ValidCase1[String], param: T): Unit
}

abstract class ValidCase1Sub extends ValidCase1[String] {
  override def method(arg: ValidCase1[String], param: String): Unit = {} // OK - this should work
}

// Variance cases should still work correctly
abstract class CovariantCase[+T] {
  def method(param: CovariantCase[String]): T
}

abstract class CovariantCaseSub extends CovariantCase[Object] {
  override def method(param: CovariantCase[String]): Object = "valid" // OK - covariance in return type
}

// Test with concrete classes too
class ConcreteBase[T] {
  def func(x: ConcreteBase[String], y: T): Unit = {}
}

class ConcreteDerived extends ConcreteBase[String] {
  override def func(x: ConcreteBase[String], y: String): Unit = {} // OK - this should work
}