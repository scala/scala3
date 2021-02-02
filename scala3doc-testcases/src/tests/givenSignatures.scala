package tests

package givenSignatures

object Obj

given Seq[String] = Nil

given GivenType = GivenType()

class GivenType

trait Ord[T]

given listOrd[T](using ord: Ord[T]): Ord[List[T]]
  = ???
