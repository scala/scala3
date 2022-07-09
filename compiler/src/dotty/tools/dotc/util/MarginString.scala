package dotty.tools.dotc.util


case class ChunkJoiner private (s: String, breakLeft: Boolean, breakRight: Boolean)
object ChunkJoiner:
  def apply(s: String) = new ChunkJoiner(s, true, true)
  def nonBreakable(s: String) = new ChunkJoiner(s, false, false)
  def leftBreakable(s: String) = new ChunkJoiner(s, breakLeft = true, false)
  def rightBreakable(s: String) = new ChunkJoiner(s, false, breakRight = true)

enum MarginString:
  case Chunk(s: String)
  case Group(strings: Vector[MarginString], 
             joiner: ChunkJoiner, 
             prefix: Option[ChunkJoiner] = None, 
             suffix: Option[ChunkJoiner] = None)

  def lines(maxWidth: Int) = MarginString.lines(this, maxWidth)

object MarginString:
  private case object Breakpoint
  
  private def tokens(str: MarginString): Vector[String | Breakpoint.type] = 
    def go(ms: MarginString): Vector[String | Breakpoint.type] = 
      ms match 
      case Chunk(s) => Vector(s)
      case g: Group => 
        import g.* 
        val b = Vector.newBuilder[String | Breakpoint.type] 

        def renderJoiner(joiner: ChunkJoiner) = 
          if joiner.breakLeft then 
            b += Breakpoint 
          b += joiner.s
          if joiner.breakRight then 
            b += Breakpoint


        prefix.foreach(renderJoiner)

        strings.zipWithIndex.foreach {case (c, idx) => 
          b ++= go(c)
          if idx != strings.size - 1 then renderJoiner(joiner)
        }

        suffix.foreach(renderJoiner)

        b.result()
    go(str)
  end tokens

  def lines(str: MarginString, maxLength: Int): Vector[String] = 
    val nonBreakable = {
      val chunks = List.newBuilder[String]
      val sb = StringBuilder()

      tokens(str).foreach {
        case str: String => sb.append(str)
        case Breakpoint => 
          chunks += sb.result()
          sb.clear()
      }

      if sb.nonEmpty then chunks += sb.result()

      chunks.result()
    }

    val lineBreaks = Vector.newBuilder[String] 
    val curLine = StringBuilder()

    nonBreakable.foreach { str =>
      if curLine.size >= maxLength then 
        lineBreaks += curLine.result()
        curLine.clear()

      curLine.append(str)
    }
    if curLine.nonEmpty then lineBreaks += curLine.result()

    lineBreaks.result()
