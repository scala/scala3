package dotty.tools.dotc
package util

import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.lang.Long.toHexString
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import core.unpickleScala2.PickleBuffer
import core.Names._

object ShowPickled {
  import core.unpickleScala2.PickleFormat._

  case class PickleBufferEntry(num: Int, startIndex: Int, tag: Int, bytes: Array[Byte]) {
    def isName: Boolean = tag == TERMname || tag == TYPEname
    def hasName: Boolean = tag match {
      case TYPEsym | ALIASsym | CLASSsym | MODULEsym | VALsym | EXTref | EXTMODCLASSref => true
      case _                                                                            => false
    }
    def readName: String =
      if (isName) new String(bytes, StandardCharsets.UTF_8)
      else sys.error("%s is no name" format tagName)
    def nameIndex: Int =
      if (hasName) readNat(bytes, 0)
      else sys.error("%s has no name" format tagName)

    def tagName: String = tag2string(tag)
    override def toString: String = "%d,%d: %s".format(num, startIndex, tagName)
  }

  case class PickleBufferEntryList(entries: IndexedSeq[PickleBufferEntry]) {
    def nameAt(idx: Int): String = {
      val entry = entries(idx)
      if (entry.isName) entry.readName
      else if (entry.hasName) entries(entry.nameIndex).readName
      else "?"
    }
  }

  def makeEntryList(buf: PickleBuffer, index: Array[Int]): PickleBufferEntryList = {
    val entries = buf.toIndexedSeq.zipWithIndex map {
      case ((tag, data), num) => PickleBufferEntry(num, index(num), tag, data)
    }

    PickleBufferEntryList(entries)
  }

  def tag2string(tag: Int): String = tag match {
    case TERMname       => "TERMname"
    case TYPEname       => "TYPEname"
    case NONEsym        => "NONEsym"
    case TYPEsym        => "TYPEsym"
    case ALIASsym       => "ALIASsym"
    case CLASSsym       => "CLASSsym"
    case MODULEsym      => "MODULEsym"
    case VALsym         => "VALsym"
    case EXTref         => "EXTref"
    case EXTMODCLASSref => "EXTMODCLASSref"
    case NOtpe          => "NOtpe"
    case NOPREFIXtpe    => "NOPREFIXtpe"
    case THIStpe        => "THIStpe"
    case SINGLEtpe      => "SINGLEtpe"
    case CONSTANTtpe    => "CONSTANTtpe"
    case TYPEREFtpe     => "TYPEREFtpe"
    case TYPEBOUNDStpe  => "TYPEBOUNDStpe"
    case REFINEDtpe     => "REFINEDtpe"
    case CLASSINFOtpe   => "CLASSINFOtpe"
    case METHODtpe      => "METHODtpe"
    case POLYtpe        => "POLYtpe"
    case IMPLICITMETHODtpe => "METHODtpe" // IMPLICITMETHODtpe no longer used.
    case SUPERtpe       => "SUPERtpe"
    case LITERALunit    => "LITERALunit"
    case LITERALboolean => "LITERALboolean"
    case LITERALbyte    => "LITERALbyte"
    case LITERALshort   => "LITERALshort"
    case LITERALchar    => "LITERALchar"
    case LITERALint     => "LITERALint"
    case LITERALlong    => "LITERALlong"
    case LITERALfloat   => "LITERALfloat"
    case LITERALdouble  => "LITERALdouble"
    case LITERALstring  => "LITERALstring"
    case LITERALnull    => "LITERALnull"
    case LITERALclass   => "LITERALclass"
    case LITERALenum    => "LITERALenum"
    case SYMANNOT       => "SYMANNOT"
    case CHILDREN       => "CHILDREN"
    case ANNOTATEDtpe   => "ANNOTATEDtpe"
    case ANNOTINFO      => "ANNOTINFO"
    case ANNOTARGARRAY  => "ANNOTARGARRAY"
    // case DEBRUIJNINDEXtpe => "DEBRUIJNINDEXtpe"
    case EXISTENTIALtpe => "EXISTENTIALtpe"
    case TREE           => "TREE"
    case MODIFIERS      => "MODIFIERS"

    case _ => "***BAD TAG***(" + tag + ")"
  }

  /** Extremely regrettably, essentially copied from PickleBuffer.
   */
  def readNat(data: Array[Byte], index: Int): Int = {
    var idx = index
    var result = 0L
    var b = 0L
    while ({
      b = data(idx)
      idx += 1
      result = (result << 7) + (b & 0x7f)
      (b & 0x80) != 0L
    })
    ()
    result.toInt
  }

  def printFile(buf: PickleBuffer, out: PrintStream = System.out.nn): Unit = {
    out.println("Version " + buf.readNat() + "." + buf.readNat())
    val index = buf.createIndex
    val entryList = makeEntryList(buf, index)
    buf.readIndex = 0

    def p(s: String) = out print s

    def printNameRef(): Unit = {
      val idx = buf.readNat()
      val name = entryList nameAt idx
      val toPrint = " %s(%s)".format(idx, name)

      out print toPrint
    }

    def printNat() = p(" " + buf.readNat())
    def printReadNat(x: Int) = p(" " + x)

    def printSymbolRef() = printNat()
    def printTypeRef() = printNat()
    def printConstantRef() = printNat()
    def printAnnotInfoRef() = printNat()
    def printConstAnnotArgRef() = printNat()
    def printAnnotArgRef() = printNat()

    def printSymInfo(end: Int, isType: Boolean): Unit = {
      printNameRef()
      printSymbolRef()
      val pflags = buf.readLongNat()
      def printFlags(privateWithin: Option[Int]) = {
        val accessBoundary = (
          for (idx <- privateWithin) yield {
            val s = entryList nameAt idx
            s"$idx($s)"
          }
        )
        val flagString = PickleBuffer.unpickleScalaFlags(pflags, isType).toString
        out.print(" %s[%s]".format(toHexString(pflags), flagString))
      }

      /** Might be info or privateWithin */
      val x = buf.readNat()
      if (buf.readIndex == end) {
        printFlags(None)
        printReadNat(x)
      }
      else {
        printFlags(Some(x))
        printTypeRef()
      }
    }

    /** Note: the entries which require some semantic analysis to be correctly
     *  interpreted are for the most part going to tell you the wrong thing.
     *  It's not so easy to duplicate the logic applied in the UnPickler.
     */
    def printEntry(i: Int): Unit = {
      buf.readIndex = index(i)
      p(s"$i,${buf.readIndex}: ")
      val tag = buf.readByte()
      out.print(tag2string(tag))
      val len = buf.readNat()
      val end = len + buf.readIndex
      p(s" $len:")
      tag match {
        case TERMname =>
          out.print(" ")
          out.print(termName(buf.bytes, buf.readIndex, len).toString)
          buf.readIndex = end
        case TYPEname =>
          out.print(" ")
          out.print(typeName(buf.bytes, buf.readIndex, len))
          buf.readIndex = end
        case TYPEsym | ALIASsym | CLASSsym | MODULEsym | VALsym =>
          printSymInfo(end, tag == TYPEsym || tag == ALIASsym || tag == CLASSsym)
          if (tag == CLASSsym && (buf.readIndex < end)) printTypeRef()
        case EXTref | EXTMODCLASSref =>
          printNameRef()
          if (buf.readIndex < end) { printSymbolRef() }
        case THIStpe =>
          printSymbolRef()
        case SINGLEtpe =>
          printTypeRef(); printSymbolRef()
        case CONSTANTtpe =>
          printTypeRef(); printConstantRef()
        case TYPEREFtpe =>
          printTypeRef(); printSymbolRef(); buf.until(end, () =>  printTypeRef())
        case TYPEBOUNDStpe =>
          printTypeRef(); printTypeRef()
        case REFINEDtpe =>
          printSymbolRef(); buf.until(end, () =>  printTypeRef())
        case CLASSINFOtpe =>
          printSymbolRef(); buf.until(end, () =>  printTypeRef())
        case METHODtpe | IMPLICITMETHODtpe =>
          printTypeRef(); buf.until(end, () =>  printTypeRef())
        case POLYtpe =>
          printTypeRef(); buf.until(end, () => printSymbolRef())
        case LITERALboolean =>
          out.print(if (buf.readLong(len) == 0L) " false" else " true")
        case LITERALbyte    =>
          out.print(" " + buf.readLong(len).toByte)
        case LITERALshort   =>
          out.print(" " + buf.readLong(len).toShort)
        case LITERALchar    =>
          out.print(" " + buf.readLong(len).toChar)
        case LITERALint     =>
          out.print(" " + buf.readLong(len).toInt)
        case LITERALlong    =>
          out.print(" " + buf.readLong(len))
        case LITERALfloat   =>
          out.print(" " + intBitsToFloat(buf.readLong(len).toInt))
        case LITERALdouble  =>
          out.print(" " + longBitsToDouble(buf.readLong(len)))
        case LITERALstring  =>
          printNameRef()
        case LITERALenum    =>
          printSymbolRef()
        case LITERALnull    =>
          out.print(" <null>")
        case LITERALclass   =>
          printTypeRef()
        case CHILDREN       =>
          printSymbolRef(); buf.until(end, () => printSymbolRef())
        case SYMANNOT       =>
          printSymbolRef(); printTypeRef(); buf.until(end, () =>  printAnnotArgRef())
        case ANNOTATEDtpe   =>
          printTypeRef(); buf.until(end, () => printAnnotInfoRef())
        case ANNOTINFO      =>
          printTypeRef(); buf.until(end, () =>  printAnnotArgRef())
        case ANNOTARGARRAY  =>
          buf.until(end, () => printConstAnnotArgRef())
        case EXISTENTIALtpe =>
          printTypeRef(); buf.until(end, () => printSymbolRef())

        case _ =>
      }
      out.println()
      if (buf.readIndex != end)
        out.println("BAD ENTRY END: computed = %d, actual = %d, bytes = %s".format(
          end, buf.readIndex, buf.bytes.slice(index(i), (end max buf.readIndex)).mkString(", ")
        ))
    }

    for (i <- 0 until index.length) printEntry(i)
  }
}

/*
 *
  def fromFile(path: String) = fromBytes(io.File(path).toByteArray)
  def fromName(name: String) = fromBytes(scalaSigBytesForPath(name) getOrElse Array())
  def fromBytes(data: => Array[Byte]): Option[PickleBuffer] =
    try Some(new PickleBuffer(data, 0, data.length))
    catch { case _: Exception => None }

  def show(what: String, pickle: PickleBuffer) = {
    Console.println(what)
    val saved = pickle.readIndex
    pickle.readIndex = 0
    printFile(pickle, Console.out)
    pickle.readIndex = saved
  }

  def main(args: Array[String]) {
    args foreach { arg =>
      (fromFile(arg) orElse fromName(arg)) match {
        case Some(pb) => show(arg + ":", pb)
        case _        => Console.println("Cannot read " + arg)
      }
    }
  }*/
