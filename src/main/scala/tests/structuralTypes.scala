package tests

package structuralTypes

class Record(elems: (String, Any)*) extends Selectable {
    val fields: Map[String, Any]
     = elems.toMap
    def selectDynamic(name: String): Any
     = fields(name)
}

type Person = Record { val name: String; val age: Int; type Height = Int; def sth(a: Int, b: String): Int }