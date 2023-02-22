object Names {       // error
  abstract class Name

  abstract class TermName extends Name:
    def toTypeName: TypeName = ???

  final class SimpleName(val start: Int, val length: Int) extends TermName
  final class TypeName(val toTermName: TermName) extends Name

  class NameTable:
    def add(index: Int, name: Name): Unit = ()
    add(0, EmptyTermName)

  var chrs: Array[Char] = new Array[Char](1024)

  val EmptyTermName: SimpleName = SimpleName(-1, 0)
  val EmptyTypeName: TypeName = EmptyTermName.toTypeName

  val nameTable = NameTable()
}
