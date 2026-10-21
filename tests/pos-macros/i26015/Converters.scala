package repro

// Not imported in Test.scala — serves as an import-suggestion candidate.
// importSuggestions finds it via rootsOnPath(repro.Crash.type) -> repro -> repro.Converters.
object Converters:
  given Conversion[Int, String] = _.toString
