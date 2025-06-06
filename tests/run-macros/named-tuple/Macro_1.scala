import language.experimental.namedTuples

import scala.quoted._
object Macro {
  transparent inline def macr(): NamedTuple.AnyNamedTuple = ${ macrImpl() }

  def macrImpl()(using Quotes) = {
    import quotes.reflect._

    val tupleNames = TypeTree.of[scala.Tuple2["stringValue", "intValue"]]
    val tupleTypes = TypeTree.of[scala.Tuple2[String, Int]]
    val tupleValues = '{("test", 10)}.asTerm

    Apply(
      TypeApply(
        Apply(
          TypeApply(
            Select.unique(Ref(defn.NamedTupleModule), "build"),
            List(tupleNames)
          ),
          List()
        ),
        List(tupleTypes)
      ),
      List(tupleValues)
    ).asExprOf[NamedTuple.AnyNamedTuple]
  }
}
