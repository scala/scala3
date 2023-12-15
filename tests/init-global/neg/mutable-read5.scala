object Names:
  class Name(val start: Int, val length: Int)
  var chrs: Array[Char] = new Array[Char](0x20000)
  def name(s: String): Name = Name(0, chrs.length) 

object StdNames:
  val AnyRef: Names.Name = Names.name("AnyRef")
  val Array: Names.Name = Names.name("Array")
  val List: Names.Name = Names.name("List")
// nopos-error: No warnings can be incurred under -Werror.