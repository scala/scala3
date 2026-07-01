// Test case for SingletonType handling in jsig
// jsig matches: case ref: SingletonType => jsig(ref.underlying)

object SingletonTypeTest:
  // Return singleton type (x.type)
  val obj = new Object
  def returnSingletonType(): obj.type = obj

  // Method that returns singleton type
  def identity[A](x: A): x.type = x

  // Singleton type with value class
  val intVal = 42
  def returnValueSingleton(): intVal.type = intVal

  // Nested object with singleton return
  object Nested:
    def returnNested(): Nested.type = Nested

  // Class parameter as singleton type
  class WithSingleton(val x: Int):
    def returnThis(): this.type = this

  // Method with singleton type in generic
  def wrapSingleton[A](a: A): A = a

  def singletonWrapped(x: String): String =
    wrapSingleton(x)

  // Singleton type in collection
  def singletonInList: List[obj.type] =
    List(obj)

  // Function returning singleton type
  def getSingletonFunction(): () => obj.type =
    () => obj

  // Two parameters of singleton type
  def singletonPair(a: obj.type, b: obj.type): Boolean =
    a eq b
