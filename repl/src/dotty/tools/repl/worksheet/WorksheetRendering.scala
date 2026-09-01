package dotty.tools.repl.worksheet

private final case class RenderedBinder(name: String, tpe: String, value: String)

private object WorksheetRendering:
  def render(
      position: WorksheetPosition,
      values: List[RenderedBinder],
      output: String,
      screenWidth: Int
  ): Option[WorksheetStatement] =
    val outputLines = output.linesIterator.toList
    val details =
      values.map(value => s"${value.name}: ${value.tpe} = ${value.value}") :::
        outputLines.map(line => s"// $line")
    val margin = math.max(20, screenWidth - position.endColumn)
    val summary =
      values match
        case Nil => outputLines.headOption.getOrElse("")
        case value :: Nil => s": ${value.tpe} = ${oneLine(value.value)}"
        case multiple =>
          multiple
            .map(value => s"${value.name}: ${value.tpe} = ${oneLine(value.value)}")
            .mkString(", ")

    if summary.isEmpty && details.isEmpty then None
    else
      val omitsDetails =
        if values.isEmpty then outputLines.sizeIs > 1
        else outputLines.nonEmpty || values.exists(_.value.contains('\n'))
      Some(
        WorksheetStatement(
          position,
          summary.take(margin),
          details.mkString("\n"),
          !omitsDetails && summary.length <= margin
        )
      )

  private def oneLine(value: String): String =
    value.replaceAll("\\s+", " ")
