import scala.language.experimental.namedTuples
import scala.deriving.*
import scala.compiletime.*

type ToString[T] = T match
  case Int => "Int"
  case String => "String"

inline def showLabelsAndTypes[Types <: Tuple, Labels <: Tuple]: List[String] =
  inline erasedValue[Types] match {
    case _: (tpe *: types) =>
      inline erasedValue[Labels] match {
        case _: (label *: labels) =>
          val labelStr = constValue[label]
          val tpeStr = constValue[ToString[tpe]]
          s"$labelStr: $tpeStr" :: showLabelsAndTypes[types, labels]
      }
    case _: EmptyTuple =>
      Nil
}

@main def Test =
  val mirror = summon[Mirror.Of[(foo: Int, bla: String)]]
  println(constValue[mirror.MirroredLabel])
  println(showLabelsAndTypes[mirror.MirroredElemTypes, mirror.MirroredElemLabels])

  val namedTuple = summon[Mirror.Of[(foo: Int, bla: String)]].fromProduct((15, "test"))
  println(namedTuple.foo)
  println(namedTuple.bla)
