package tests

package structuralTypes

type Person = Record { val name: String; val age: Int; type Height = Int; def sth(a: Int, b: String): Int; }

type R = { type T; val x: Int; type U <: this.T; def foo(): Int; }

class Record(elems: (String, Any)*) extends Selectable {
  val fields: Map[String, Any]
   = elems.toMap
  def selectDynamic(name: String): Any
   = fields(name)
}