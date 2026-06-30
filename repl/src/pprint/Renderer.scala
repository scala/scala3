package pprint
object Renderer{
  /**
    * Basically like mkString, but for nested iterators. Used whenever
    * you want to put stuff "in between" the elements of the larger
    * iterator
    */
  def joinIter[T](it0: Iterator[Iterator[T]], joiner: => Iterator[T]) = {
    new Util.ConcatIterator(it0, () => joiner)
  }

  val openParen = fansi.Str("(")
  val closeParen = fansi.Str(")")
  val commaSpace = fansi.Str(", ")
  val newLine = fansi.Str("\n")
  val commaNewLine = fansi.Str(",\n")
  private val cachedIndents = Array.tabulate(64)(n => fansi.Str(" " * n))
  def indent(n: Int) = if (n < 64) cachedIndents(n) else fansi.Str(" " * n)
}
class Renderer(maxWidth: Int,
               colorApplyPrefix: fansi.Attrs,
               colorLiteral: fansi.Attrs,
               indentStep: Int){



  def rec(x: Tree, leftOffset: Int, indentCount: Int): Result = x match{
    case Tree.Apply(prefix, body) =>
      val nonEmpty = body.hasNext
      // Render children and buffer them until you fill up a single line,
      // or you run out of children.
      //
      // Even before rendering any children, the indentation, prefix
      // and the two open/close parens already take up a few characters
      var totalHorizontalWidth = leftOffset + prefix.length + 2
      val buffer = collection.mutable.Buffer.empty[collection.Seq[fansi.Str]]
      var lastChildIter = Iterator[fansi.Str]()
      var childCompletedLineCount = 0
      while(body.hasNext && totalHorizontalWidth <= maxWidth && childCompletedLineCount == 0){

        val child = body.next()
        val childRes = rec(child, (indentCount + 1) * indentStep, indentCount + 1)

        val childBuffer = collection.mutable.Buffer.empty[fansi.Str]
        while(childRes.iter.hasNext && totalHorizontalWidth < maxWidth){
          val next = childRes.iter.next()
          childBuffer += next
          totalHorizontalWidth += next.length
        }


        if (body.hasNext) {
          totalHorizontalWidth += 2
        }

        if (!childRes.iter.hasNext){
          childCompletedLineCount = childCompletedLineCount + childRes.completedLineCount
        }else{
          lastChildIter = childRes.iter

        }

        buffer += childBuffer.toSeq
      }

      def applyHeader = Iterator(colorApplyPrefix(prefix), Renderer.openParen)

      val indentPlusOne = Renderer.indent((indentCount + 1) * indentStep)

      def separator = Iterator(Renderer.commaNewLine, indentPlusOne)

      if (
        totalHorizontalWidth <= maxWidth &&
        childCompletedLineCount == 0 &&
        !lastChildIter.hasNext
      ) {
        val iter = Util.concat(
          () => applyHeader,
          () => Renderer.joinIter(
            buffer.iterator.map(_.iterator),
            Iterator(Renderer.commaSpace)
          ),
          () => Iterator(Renderer.closeParen)
        )

        val length: Int = buffer.iterator.map(_.iterator.map(_.length).sum).sum
        new Result(iter, 0, length)
      }else if (!nonEmpty && totalHorizontalWidth > maxWidth) {
        val iter = Util.concat(
          () => applyHeader,
          () => Iterator(
            Renderer.newLine,
            Renderer.indent(indentCount * indentStep),
            Renderer.closeParen
          )
        )

        val length: Int = buffer.iterator.map(_.iterator.map(_.length).sum).sum
        new Result(iter, 0, length)
      } else {
        def bufferedFragments = Renderer.joinIter(
          for((v, i) <- buffer.iterator.zipWithIndex) yield{
            if (i < buffer.length-1) v.iterator
            else v.iterator ++ lastChildIter
          },
          separator
        )

        def nonBufferedFragments = Renderer.joinIter(
          body.map(c => rec(c, (indentCount + 1) * indentStep, indentCount + 1).iter),
          separator
        )

        def allFragments =
          if (buffer.isEmpty) nonBufferedFragments
          else if (!body.hasNext) bufferedFragments
          else Renderer.joinIter(Iterator(bufferedFragments, nonBufferedFragments), separator)

        def iter = Util.concat(
          () => applyHeader,
          () => Iterator(Renderer.newLine, indentPlusOne),
          () => allFragments,
          () => Iterator(
            Renderer.newLine,
            Renderer.indent(indentCount * indentStep),
            Renderer.closeParen
          )
        )


        new Result(iter, childCompletedLineCount + 2, indentCount * indentStep + 1)
      }

    case Tree.Infix(lhs, op, rhs) =>
      rec(lhs, leftOffset, indentCount).flatMap{ (lhsNewline, lhsLastLineLength) =>
        Result.fromString(" " + op + " ").flatMap((_, _) =>
          rec(rhs, lhsLastLineLength, indentCount)
        )
      }

    case t: Tree.Lazy =>

      lazy val str = t.body0(Tree.Ctx(
        maxWidth, leftOffset, indentCount,
        indentStep, colorLiteral, colorApplyPrefix
      ))
      new Truncated(str.map(fansi.Str(_)), maxWidth, height = 99999999).toResult

    case t: Tree.Literal => Result.fromString(colorLiteral(t.body))

    case Tree.KeyValue(k, v) =>
      val prefix = s"$k = "
      Result.fromString(prefix)
        .flatMap((_, _) => rec(v, leftOffset + prefix.length, indentCount))

  }
}

