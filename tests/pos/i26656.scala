
object ParsingIntra {
    val ImmutLexerFlow: ImmutLexerFlowModule = new AnyRef with ImmutLexerFlowModule
    trait ImmutLexerFlowModule {

        opaque type NvriImpl >: String <: String = String

        transparent inline def Parser: Parser1.type = Parser1

        object Parser1

        extension (C: Parser1.type) {
            def gc(): Unit = { }
        }
    }
}

object Parsing {
    def gcRules() = locally[Unit]:
        ParsingIntra.ImmutLexerFlow.Parser.gc()
}
