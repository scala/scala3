val foo1 = new Selectable:
  type Fields = (xyz: Int)
  def selectDynamic(name: String): Any = 23
val _ = foo1.xyz // error


