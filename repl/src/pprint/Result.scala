package pprint

import scala.annotation.tailrec


/**
  * The intermediate return type of the pretty-print system: provides an
  * iterator which produces the actual string output, as well as metadata
  * around that output that is only available after the iterator is exhausted
  */
class Result(val iter: Iterator[fansi.Str],
             completedLineCount0: => Int,
             lastLineLength0: => Int){
  lazy val completedLineCount = {
    require(iter.isEmpty)
    completedLineCount0
  }
  lazy val lastLineLength = {
    require(iter.isEmpty)
    lastLineLength0
  }
  def flatMap(f: (Int, Int) => Result): Result = {
    var newCompletedLineCount = 0
    var newLastLineLength = 0

    val mergedIterator = Util.concat(
      () => iter,
      () => {
        require(!iter.hasNext)
        val newResult = f(completedLineCount, lastLineLength0)
        newResult.iter.map{ x =>
          if (!newResult.iter.hasNext){
            newCompletedLineCount = newResult.completedLineCount
            newLastLineLength = newResult.lastLineLength
          }
          x
        }
      }
    )
    new Result(
      mergedIterator,
      newCompletedLineCount + completedLineCount,
      if (newCompletedLineCount > 0) newLastLineLength
      else newLastLineLength + lastLineLength
    )

  }
}

object Result{
  def fromString(s: => fansi.Str) = {
    lazy val lines = s.plainText.linesIterator.toArray
    new Result(Iterator(s), lines.length - 1, lines.last.length)
  }
}
