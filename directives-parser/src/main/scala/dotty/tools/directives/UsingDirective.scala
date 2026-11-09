package dotty.tools.directives

/** A single parsed `//> using` directive. */
case class UsingDirective(
  key: String,
  values: Seq[DirectiveValue],
  keyPosition: Position
)

/** The result of parsing a source file for using directives. */
case class UsingDirectivesResult(
  directives: Seq[UsingDirective],
  codeOffset: Int,
  diagnostics: Seq[UsingDirectiveDiagnostic]
)
