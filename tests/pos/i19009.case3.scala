trait Bound[+E]

trait SegmentT[E, +S]
object SegmentT:
  trait WithPrev[E, +S] extends SegmentT[E, S]

trait SegmentSeqT[E, +S]:
  def getSegmentForBound(bound: Bound[E]): SegmentT[E, S] with S

abstract class AbstractSegmentSeq[E, +S] extends SegmentSeqT[E, S]

trait MappedSegmentBase[E, S]

type MappedSegment[E, S] = AbstractMappedSegmentSeq.MappedSegment[E, S]

object AbstractMappedSegmentSeq:
  type MappedSegment[E, S] = SegmentT[E, MappedSegmentBase[E, S]] with MappedSegmentBase[E, S]

abstract class AbstractMappedSegmentSeq[E, S]
    extends AbstractSegmentSeq[E, MappedSegmentBase[E, S]]:
  def originalSeq: SegmentSeqT[E, S]

  final override def getSegmentForBound(bound: Bound[E]): MappedSegment[E, S] =
    searchFrontMapper(frontMapperGeneral, originalSeq.getSegmentForBound(bound))

  protected final def frontMapperGeneral(original: SegmentT[E, S]): MappedSegment[E, S] = ???

  protected def searchFrontMapper[Seg >: SegmentT.WithPrev[E, S] <: SegmentT[E, S], R](
      mapper: Seg => R,
      original: Seg
  ): R = ???
