class SourceFile

object Contexts:
  val NoContext: Context = new Context
  class Context:
    private var _source: SourceFile = null
    final def source: SourceFile = _source // warn
    def setSource(source: SourceFile) = {
      this._source = source
    }

object Implicits:
  import Contexts.*
  case class SearchFailure(tag: Int, source: SourceFile)
  val NoMatchingFailure: SearchFailure = SearchFailure(1, NoContext.source)