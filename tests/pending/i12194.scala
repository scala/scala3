import scala.annotation.implicitNotFound
import scala.compiletime.package$package.summonAll
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.NotGiven
import scala.deriving.*

def f(): Unit =
  var t = (??? : Tuple1[ValueOf["foo"]]); t.toList.map(identity)
  (??? : Tuple1[ValueOf["foo"]]).toList.map(identity)

@main def Test(): Unit =
  println(summonAll[Tuple.Map[("foo", "bar"), ValueOf]].toList.map{
    case str: ValueOf[_] ⇒ str.value
  })