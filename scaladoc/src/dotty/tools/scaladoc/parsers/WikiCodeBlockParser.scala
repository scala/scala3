package dotty.tools.scaladoc.parsers

import com.vladsch.flexmark.ast._
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.parser.core._
import com.vladsch.flexmark.parser.block._
import com.vladsch.flexmark.util.ast.Block
import com.vladsch.flexmark.util.ast.BlockContent
import com.vladsch.flexmark.util.data.DataHolder
import com.vladsch.flexmark.util.sequence.BasedSequence
import com.vladsch.flexmark.util.sequence.SegmentedSequence

import java.{util => ju}
import ju.regex.Matcher
import ju.regex.Pattern
import scala.jdk.CollectionConverters._


/** Copied from FencedCodeBlockParser. */
object WikiCodeBlockParser {
  private val OPENING_FENCE = Pattern.compile("^\\{{3}")
  private val CLOSING_FENCE = Pattern.compile("^(\\}{3})(?=[ \t]*$)$")

  class Factory extends CustomBlockParserFactory {
    override def apply(options: DataHolder): BlockParserFactory =
      new WikiCodeBlockParser.BlockFactory(options)

    override def getAfterDependents =
      new ju.HashSet[Class[?]](ju.Arrays.asList(
        classOf[BlockQuoteParser.Factory],
        classOf[HeadingParser.Factory],
        //FencedCodeBlockParser.Factory.class,
        //HtmlBlockParser.Factory.class,
        //ThematicBreakParser.Factory.class,
        //ListBlockParser.Factory.class,
        //IndentedCodeBlockParser.Factory.class
      ))

    override def getBeforeDependents =
      new ju.HashSet[Class[?]](ju.Arrays.asList(
        //BlockQuoteParser.Factory.class,
        //HeadingParser.Factory.class,
        //FencedCodeBlockParser.Factory.class,
        classOf[HtmlBlockParser.Factory],
        classOf[ThematicBreakParser.Factory],
        classOf[ListBlockParser.Factory],
        classOf[IndentedCodeBlockParser.Factory],
      ))

    override def affectsGlobalScope = false
  }

  private[WikiCodeBlockParser] class BlockFactory (val options: DataHolder)
  extends AbstractBlockParserFactory(options) {
    def tryStart(state: ParserState, matchedBlockParser: MatchedBlockParser): BlockStart = {
      val nextNonSpace = state.getNextNonSpaceIndex
      val line = state.getLine
      if state.getIndent < 4 then {
        val trySequence = line.subSequence(nextNonSpace, line.length)
        val matcher = OPENING_FENCE.matcher(trySequence)
        if matcher.find then {
          val fenceLength = matcher.group(0).length
          val blockParser =
            new WikiCodeBlockParser(state.getProperties, fenceLength, state.getIndent, nextNonSpace)
          blockParser.block.setOpeningMarker(trySequence.subSequence(0, fenceLength))
          return BlockStart.of(blockParser).atIndex(nextNonSpace + fenceLength)
        }
      }
      BlockStart.none
    }
  }
}

/** Copied from FencedCodeBlockParser. */
class WikiCodeBlockParser(
  options: DataHolder,
  var fenceLength: Int,
  private var fenceIndent: Int,
  private var fenceMarkerIndent: Int
) extends AbstractBlockParser {

  this.fenceMarkerIndent = fenceIndent + fenceMarkerIndent

  final private val block = new FencedCodeBlock()
  private var content = new BlockContent
  private val codeContentBlock = Parser.FENCED_CODE_CONTENT_BLOCK.get(options)

  def getBlock: Block = block
  def getFenceIndent: Int = fenceIndent
  def getFenceMarkerIndent: Int = fenceMarkerIndent
  def tryContinue(state: ParserState): BlockContinue = {
    val nextNonSpace = state.getNextNonSpaceIndex
    var newIndex = state.getIndex
    val line = state.getLine
    val matches =
      state.getIndent <= 3
      && nextNonSpace < line.length

    if matches then {
      val trySequence = line.subSequence(nextNonSpace, line.length)
      val matcher = WikiCodeBlockParser.CLOSING_FENCE.matcher(trySequence)
      if matcher.find then {
        val foundFenceLength = matcher.group(0).length
        if (foundFenceLength >= fenceLength) { // closing fence - we're at end of line, so we can finalize now
          block.setClosingMarker(trySequence.subSequence(0, foundFenceLength))
          return BlockContinue.finished
        }
      }
    }
    // skip optional spaces of fence indent
    var i = fenceIndent
    while ({
      i > 0 && newIndex < line.length && line.charAt(newIndex) == ' '
    }) do {
      newIndex += 1
      i -= 1
    }
    BlockContinue.atIndex(newIndex)
  }

  override def addLine(state: ParserState, line: BasedSequence): Unit = {
    content.add(line, state.getIndent)
  }

  override def isPropagatingLastBlankLine(lastMatchedBlockParser: BlockParser) = false

  override def closeBlock(state: ParserState): Unit = { // first line, if not blank, has the info string
    val lines = content.getLines
    if (lines.size > 0) {
      val info = lines.get(0)
      if (!info.isBlank) block.setInfo(info.trim)
      val chars = content.getSpanningChars
      val spanningChars = chars.baseSubSequence(chars.getStartOffset, lines.get(0).getEndOffset)
      if (lines.size > 1) { // have more lines
        val segments = lines.subList(1, lines.size)
        block.setContent(spanningChars, segments)
        if this.codeContentBlock then {
          val codeBlock = new CodeBlock()
          codeBlock.setContent(segments)
          codeBlock.setCharsFromContent
          block.appendChild(codeBlock)
        } else {
          val codeBlock = new Text(SegmentedSequence.create(segments.asScala.toSeq*))
          block.appendChild(codeBlock)
        }
      }
      else block.setContent(spanningChars, BasedSequence.EMPTY_LIST)
    }
    else block.setContent(content)
    block.setCharsFromContent
    content = null.asInstanceOf[BlockContent] // release for GC
  }
}

