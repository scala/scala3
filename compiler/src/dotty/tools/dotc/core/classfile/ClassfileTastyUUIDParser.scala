package dotty.tools.dotc
package core.classfile

import scala.language.unsafeNulls
import scala.compiletime.uninitialized

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.*
import dotty.tools.io.AbstractFile
import dotty.tools.tasty.{ TastyReader, UnpickleException }

import ClassfileParser.Header

import java.io.IOException
import java.lang.Integer.toHexString
import java.util.UUID

class ClassfileTastyUUIDParser(classfile: AbstractFile)(ictx: Context) {

  import ClassfileConstants.*

  private var pool: ConstantPool = uninitialized // the classfile's constant pool
  private var classfileVersion: Header.Version = Header.Version.Unknown

  def checkTastyUUID(tastyUUID: UUID)(using Context): Unit = try ctx.base.reusableDataReader.withInstance { reader =>
    implicit val reader2 = reader.reset(classfile)
    this.classfileVersion = ClassfileParser.parseHeader(classfile)
    this.pool = new ConstantPool
    checkTastyAttr(tastyUUID)
    this.pool =  null
  }
  catch {
    case e: RuntimeException =>
      if (ctx.debug) e.printStackTrace()
      val addendum = e match
        case _: UnpickleException => ""
        case _ => Header.Version.brokenVersionAddendum(classfileVersion)
      throw new IOException(
        i"""  class file ${classfile.canonicalPath} is broken$addendum,
          |  reading aborted with ${e.getClass}:
          |  ${Option(e.getMessage).getOrElse("")}""")
  }

  private def checkTastyAttr(tastyUUID: UUID)(using ctx: Context, in: DataReader): Unit = {
    in.nextChar // jflags
    in.nextChar // nameIdx
    skipSuperclasses()
    skipMembers() // fields
    skipMembers() // methods
    val attrs = in.nextChar
    val attrbp = in.bp

    def scan(target: TypeName): Boolean = {
      in.bp = attrbp
      var i = 0
      while (i < attrs && pool.getName(in.nextChar).name.toTypeName != target) {
        val attrLen = in.nextInt
        in.skip(attrLen)
        i += 1
      }
      i < attrs
    }

    if (scan(tpnme.TASTYATTR)) {
      val attrLen = in.nextInt
      val bytes = in.nextBytes(attrLen)
      if (attrLen == 16) { // A tasty attribute with that has only a UUID (16 bytes) implies the existence of the .tasty file
        val expectedUUID =
          val reader = new TastyReader(bytes, 0, 16)
          new UUID(reader.readUncompressedLong(), reader.readUncompressedLong())
        if (expectedUUID != tastyUUID)
          report.warning(s"$classfile is out of sync with its TASTy file. Loaded TASTy file. Try cleaning the project to fix this issue", NoSourcePosition)
      }
      else
        // Before 3.0.0 we had a mode where we could embed the TASTY bytes in the classfile. This has not been supported in any stable release.
        report.error(s"Found a TASTY attribute with a length different from 16 in $classfile. This is likely a bug in the compiler. Please report.", NoSourcePosition)
    }

  }

  private def skipAttributes()(using in: DataReader): Unit = {
    val attrCount = in.nextChar
    for (i <- 0 until attrCount) {
      in.skip(2); in.skip(in.nextInt)
    }
  }

  private def skipMembers()(using in: DataReader): Unit = {
    val memberCount = in.nextChar
    for (i <- 0 until memberCount) {
      in.skip(6); skipAttributes()
    }
  }

  private def skipSuperclasses()(using in: DataReader): Unit = {
    in.skip(2) // superclass
    val ifaces = in.nextChar
    in.skip(2 * ifaces)
  }

  class ConstantPool(using in: DataReader) extends ClassfileParser.AbstractConstantPool {
    def getClassOrArrayType(index: Int)(using ctx: Context, in: DataReader): Type = throw new UnsupportedOperationException
    def getClassSymbol(index: Int)(using ctx: Context, in: DataReader): Symbol = throw new UnsupportedOperationException
    def getType(index: Int, isVarargs: Boolean)(using x$3: Context, x$4: DataReader): Type = throw new UnsupportedOperationException
  }
}
