import scala.language.experimental.namedTuples
import scala.deriving.*
import scala.compiletime.*

@main def Test =
  summon[Mirror.SumOf[(foo: Int, bla: String)]] // error
  val namedTuple = summon[Mirror.Of[(foo: Int, bla: String)]{
    type MirroredElemLabels = ("foo", "ba")
  }]// error

