package dotty.tools.pc

import scala.meta.internal.metals.ReportContext
import scala.meta.internal.metals.Reporter
import scala.meta.internal.metals.EmptyReportContext

class PcReportContext(underlying: ReportContext, pcDetails: Map[String, String]) extends ReportContext:
  override def incognito: Reporter = underlying.incognito
  override def unsanitized: Reporter = underlying.unsanitized
  override def bloop: Reporter = underlying.bloop
  def additionalData: String = pcDetails.mkString("\n")

object PcReportContext:
  def empty: PcReportContext = PcReportContext(EmptyReportContext, Map.empty)

