package dotty.tools.repl.worksheet

private enum RenderedBinder:
  case Value(name: String, tpe: String, value: String)
  case Declaration(text: String)

private object WorksheetRendering:
  def render(
      position: WorksheetPosition,
      values: List[RenderedBinder],
      output: String,
      screenWidth: Int
  ): Option[WorksheetStatement] =
    val outputLines = output.linesIterator.toList
    val details =
      values.map(detailsOf) ::: outputLines.map(line => s"// $line")
    val margin = math.max(20, screenWidth - position.endColumn)
    val summary =
      values match
        case Nil => outputLines.headOption.getOrElse("")
        case value :: Nil => summaryOf(value, includeName = false)
        case multiple =>
          multiple.map(value => summaryOf(value, includeName = true)).mkString(", ")

    if summary.isEmpty && details.isEmpty then None
    else
      val omitsDetails =
        if values.isEmpty then outputLines.sizeIs > 1
        else outputLines.nonEmpty || values.exists(losesFormatting)
      Some(
        WorksheetStatement(
          position,
          summary.take(margin),
          details.mkString("\n"),
          !omitsDetails && summary.length <= margin
        )
      )

  private def detailsOf(binder: RenderedBinder): String = binder match
    case RenderedBinder.Value(name, tpe, value) => s"$name: $tpe = $value"
    case RenderedBinder.Declaration(text) => text

  private def summaryOf(binder: RenderedBinder, includeName: Boolean): String = binder match
    case RenderedBinder.Value(name, tpe, value) =>
      val namePrefix = if includeName then s"$name: " else ": "
      s"$namePrefix$tpe = ${oneLine(value)}"
    case RenderedBinder.Declaration(text) => text

  private def losesFormatting(binder: RenderedBinder): Boolean = binder match
    case RenderedBinder.Value(_, _, value) => oneLine(value) != value
    case RenderedBinder.Declaration(text) => oneLine(text) != text

  private def oneLine(value: String): String =
    value.replaceAll("\\s+", " ")
