trait RType
trait TypeEntry{
  def typeAdapter: TypeAdapter[?] = ???
}

trait TypeAdapter[E]
case class OptionTypeAdapter[E](nullIsNone: Boolean, valueTypeAdapter: TypeAdapter[E]) extends TypeAdapter[E]
case class JavaOptionalTypeAdapter[E](nullIsNone: Boolean, valueTypeAdapter: TypeAdapter[E]) extends TypeAdapter[E]

def typeAdapterOf(concreteType: RType): TypeAdapter[?] = ???

@main def test() = {
  // https://github.com/gzoller/scalajack/blob/4a29366e28fbd594d7c21b4eb969ca14626ac0d1/core/src/main/scala/co.blocke.scalajack/typeadapter/TupleTypeAdapter.scala#L21-L30
  val seq = List.empty[RType]
  seq map { f =>
    typeAdapterOf(f) match {
      case ota: OptionTypeAdapter[?] => ota.copy(nullIsNone = true)
      case jota: JavaOptionalTypeAdapter[?] => jota.copy(nullIsNone = true)
      case other => other
    }
  }
}

// 15154