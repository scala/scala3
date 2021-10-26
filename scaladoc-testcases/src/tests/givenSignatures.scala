package tests

package givenSignatures

object Obj

given Seq[String] = Nil //expected: given given_Seq_String: Seq[String]

given GivenType = GivenType() //expected: given given_GivenType: GivenType

class GivenType

trait Ord[T]

given listOrd[T](using ord: Ord[T]): Ord[List[T]]
  = ???

trait Foo[A]

given listOrd: Foo[String] with { val i: Int = 1 } //expected: given listOrd: listOrd.type

trait Placeholder //expected: object listOrd extends Foo[String]
