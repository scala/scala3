import language.experimental.namedTypeArguments
package namedTypeArgsR {

object t1 {

  def construct[Elem, Coll[_]](xs: Elem*): Coll[Elem] = ???

  val xs3 = construct[Coll = List](1, 2, 3)

  val xs2 = construct[Coll = List, Elem = Int](1, 2, 3)

}

}
