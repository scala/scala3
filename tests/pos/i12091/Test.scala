import foo.Model

class Property[T] {
  def value: T = ???
  def get(): T = ???
}

class Test {
  def ko(model: Property[Model]):  Unit = model.value.clear()    // used to fail: Unable to emit reference to method clear in class ModelBase

  def ok(model: Model):            Unit = model.clear()

  def ok1(model: Property[Model]): Unit = model.get().clear()

  def ok2(model: Property[Model]): Unit = { val x = model.value; x.clear() }
}
