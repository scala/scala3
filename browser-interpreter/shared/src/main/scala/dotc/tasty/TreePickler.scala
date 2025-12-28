package dotc.tasty

import dotc.core._
import Names._
import Types._
import Flags._
import Symbols._
import Constants._
import Contexts._
import dotc.ast.Trees._
import tasty.TastyFormat._
import tasty.TastyBuffer._

import scala.collection.mutable

/**
 * Cross-platform TASTy pickler for the browser compiler.
 *
 * This serializes typed trees into the TASTy format.
 */
class TreePickler {

  /** The output buffer */
  private val buf = new TastyPickleBuffer(1024)

  /** Name table */
  private val nameTable = mutable.LinkedHashMap[Name, Int]()
  private var nextNameIndex = 1

  /** Address mapping for sharing */
  private val treeAddrs = mutable.HashMap[Tree, Addr]()
  private val typeAddrs = mutable.HashMap[Type, Addr]()

  /** Pickle a compilation unit */
  def pickle(trees: List[Tree])(using ctx: Context): Array[Byte] = {
    // First, collect all names
    trees.foreach(collectNames)

    // Write header
    writeHeader()

    // Write name table section
    writeNameTable()

    // Write AST section
    writeASTSection(trees)

    buf.bytes.take(buf.length)
  }

  private def collectNames(tree: Tree)(using ctx: Context): Unit = tree match {
    case id: Ident => registerName(id.name)
    case sel: Select => registerName(sel.name); collectNames(sel.qualifier)
    case valDef: ValDef => registerName(valDef.name); collectNames(valDef.tpt); collectNames(valDef.rhs)
    case defDef: DefDef =>
      registerName(defDef.name)
      defDef.paramss.foreach {
        case TermParamClause(params) => params.foreach(collectNames)
        case TypeParamClause(params) => params.foreach(collectNames)
      }
      collectNames(defDef.tpt)
      collectNames(defDef.rhs)
    case typeDef: TypeDef => registerName(typeDef.name); collectNames(typeDef.rhs)
    case app: Apply => collectNames(app.fun); app.args.foreach(collectNames)
    case tapp: TypeApply => collectNames(tapp.fun); tapp.args.foreach(collectNames)
    case block: Block => block.stats.foreach(collectNames); collectNames(block.expr)
    case ifExpr: If => collectNames(ifExpr.cond); collectNames(ifExpr.thenp); collectNames(ifExpr.elsep)
    case matchExpr: Match => collectNames(matchExpr.selector); matchExpr.cases.foreach(c => {
      collectNames(c.pat); collectNames(c.guard); collectNames(c.body)
    })
    case fn: Function => fn.args.foreach(collectNames); collectNames(fn.body)
    case pkg: PackageDef => collectNames(pkg.pid); pkg.stats.foreach(collectNames)
    case template: Template =>
      collectNames(template.constr)
      template.parents.foreach(collectNames)
      template.body.foreach(collectNames)
    case _ => // No names to collect
  }

  private def registerName(name: Name): Int = {
    nameTable.getOrElseUpdate(name, {
      val idx = nextNameIndex
      nextNameIndex += 1
      idx
    })
  }

  private def writeHeader(): Unit = {
    // Write magic number
    header.foreach(b => buf.writeByte(b))

    // Write version
    buf.writeNat(MajorVersion)
    buf.writeNat(MinorVersion)
    buf.writeNat(ExperimentalVersion)

    // Write tooling version
    val tooling = "browser-compiler-0.1.0"
    buf.writeNat(tooling.length)
    tooling.getBytes("UTF-8").foreach(buf.writeByte(_))

    // Write UUID (random bytes)
    for (_ <- 0 until 16) buf.writeByte(0)
  }

  private def writeNameTable(): Unit = {
    val nameStart = buf.currentAddr

    // Calculate name table size
    val nameBuf = new TastyPickleBuffer(1024)
    nameTable.foreach { case (name, _) =>
      val str = name.toString
      nameBuf.writeByte(NameTags.UTF8)
      nameBuf.writeNat(str.length)
      str.getBytes("UTF-8").foreach(nameBuf.writeByte(_))
    }

    // Write name table length
    buf.writeNat(nameBuf.length)

    // Copy name bytes
    for (i <- 0 until nameBuf.length) {
      buf.writeByte(nameBuf.bytes(i) & 0xFF)
    }
  }

  private def writeASTSection(trees: List[Tree])(using ctx: Context): Unit = {
    // Write section name reference
    val astsSectionName = registerName(termName(ASTsSection))
    buf.writeNat(astsSectionName)

    // Write AST bytes to temp buffer
    val astBuf = new TastyPickleBuffer(4096)
    trees.foreach(tree => pickleTree(tree, astBuf))

    // Write section length and content
    buf.writeNat(astBuf.length)
    for (i <- 0 until astBuf.length) {
      buf.writeByte(astBuf.bytes(i) & 0xFF)
    }
  }

  private def pickleTree(tree: Tree, buf: TastyPickleBuffer)(using ctx: Context): Unit = tree match {
    case lit: Literal => pickleLiteral(lit.const, buf)
    case id: Ident => pickleIdent(id, buf)
    case sel: Select => pickleSelect(sel, buf)
    case app: Apply => pickleApply(app, buf)
    case block: Block => pickleBlock(block, buf)
    case valDef: ValDef => pickleValDef(valDef, buf)
    case defDef: DefDef => pickleDefDef(defDef, buf)
    case pkg: PackageDef => picklePackageDef(pkg, buf)
    case EmptyTree => // Nothing to write
    case _ => // Skip unhandled trees for now
  }

  private def pickleLiteral(const: Constant, buf: TastyPickleBuffer)(using ctx: Context): Unit = const.tag match {
    case UnitTag => buf.writeByte(UNITconst)
    case BooleanTag => buf.writeByte(if (const.booleanValue) TRUEconst else FALSEconst)
    case ByteTag => buf.writeByte(BYTEconst); buf.writeInt(const.byteValue)
    case ShortTag => buf.writeByte(SHORTconst); buf.writeInt(const.shortValue)
    case CharTag => buf.writeByte(CHARconst); buf.writeNat(const.charValue)
    case IntTag => buf.writeByte(INTconst); buf.writeInt(const.intValue)
    case LongTag => buf.writeByte(LONGconst); buf.writeLongInt(const.longValue)
    case FloatTag => buf.writeByte(FLOATconst); buf.writeInt(java.lang.Float.floatToIntBits(const.floatValue))
    case DoubleTag => buf.writeByte(DOUBLEconst); buf.writeLongInt(java.lang.Double.doubleToLongBits(const.doubleValue))
    case StringTag =>
      buf.writeByte(STRINGconst)
      buf.writeNat(nameTable(termName(const.stringValue)))
    case NullTag => buf.writeByte(NULLconst)
    case _ => // Skip unsupported constant types
  }

  private def pickleIdent(id: Ident, buf: TastyPickleBuffer)(using ctx: Context): Unit = {
    buf.writeByte(IDENT)
    buf.writeNat(nameTable(id.name))
    // Type reference would go here
  }

  private def pickleSelect(sel: Select, buf: TastyPickleBuffer)(using ctx: Context): Unit = {
    buf.writeByte(SELECT)
    buf.writeNat(nameTable(sel.name))
    pickleTree(sel.qualifier, buf)
  }

  private def pickleApply(app: Apply, buf: TastyPickleBuffer)(using ctx: Context): Unit = {
    val applyBuf = new TastyPickleBuffer(256)
    pickleTree(app.fun, applyBuf)
    app.args.foreach(arg => pickleTree(arg, applyBuf))

    buf.writeByte(APPLY)
    buf.writeNat(applyBuf.length)
    for (i <- 0 until applyBuf.length) buf.writeByte(applyBuf.bytes(i) & 0xFF)
  }

  private def pickleBlock(block: Block, buf: TastyPickleBuffer)(using ctx: Context): Unit = {
    val blockBuf = new TastyPickleBuffer(512)
    pickleTree(block.expr, blockBuf)
    block.stats.foreach(stat => pickleTree(stat, blockBuf))

    buf.writeByte(BLOCK)
    buf.writeNat(blockBuf.length)
    for (i <- 0 until blockBuf.length) buf.writeByte(blockBuf.bytes(i) & 0xFF)
  }

  private def pickleValDef(valDef: ValDef, buf: TastyPickleBuffer)(using ctx: Context): Unit = {
    val vdBuf = new TastyPickleBuffer(256)
    vdBuf.writeNat(nameTable(valDef.name))
    pickleTree(valDef.tpt, vdBuf)
    pickleTree(valDef.rhs, vdBuf)
    pickleModifiers(valDef.mods, vdBuf)

    buf.writeByte(VALDEF)
    buf.writeNat(vdBuf.length)
    for (i <- 0 until vdBuf.length) buf.writeByte(vdBuf.bytes(i) & 0xFF)
  }

  private def pickleDefDef(defDef: DefDef, buf: TastyPickleBuffer)(using ctx: Context): Unit = {
    val ddBuf = new TastyPickleBuffer(512)
    ddBuf.writeNat(nameTable(defDef.name))
    // Params and return type would go here
    pickleTree(defDef.tpt, ddBuf)
    pickleTree(defDef.rhs, ddBuf)
    pickleModifiers(defDef.mods, ddBuf)

    buf.writeByte(DEFDEF)
    buf.writeNat(ddBuf.length)
    for (i <- 0 until ddBuf.length) buf.writeByte(ddBuf.bytes(i) & 0xFF)
  }

  private def picklePackageDef(pkg: PackageDef, buf: TastyPickleBuffer)(using ctx: Context): Unit = {
    val pkgBuf = new TastyPickleBuffer(1024)
    pickleTree(pkg.pid, pkgBuf)
    pkg.stats.foreach(stat => pickleTree(stat, pkgBuf))

    buf.writeByte(PACKAGE)
    buf.writeNat(pkgBuf.length)
    for (i <- 0 until pkgBuf.length) buf.writeByte(pkgBuf.bytes(i) & 0xFF)
  }

  private def pickleModifiers(mods: Modifiers, buf: TastyPickleBuffer): Unit = {
    if (mods.is(Private)) buf.writeByte(PRIVATE)
    if (mods.is(Protected)) buf.writeByte(PROTECTED)
    if (mods.is(Abstract)) buf.writeByte(ABSTRACT)
    if (mods.is(Final)) buf.writeByte(FINAL)
    if (mods.is(Sealed)) buf.writeByte(SEALED)
    if (mods.is(Case)) buf.writeByte(CASE)
    if (mods.is(Implicit)) buf.writeByte(IMPLICIT)
    if (mods.is(Lazy)) buf.writeByte(LAZY)
    if (mods.is(Override)) buf.writeByte(OVERRIDE)
    if (mods.is(Inline)) buf.writeByte(INLINE)
    if (mods.is(Mutable)) buf.writeByte(MUTABLE)
  }
}

/** A buffer for writing TASTy bytes */
class TastyPickleBuffer(initialSize: Int) {
  var bytes: Array[Byte] = new Array[Byte](initialSize)
  var length: Int = 0

  def currentAddr: Addr = Addr(length)

  private def ensureCapacity(needed: Int): Unit = {
    if (length + needed > bytes.length) {
      val newSize = math.max(bytes.length * 2, length + needed)
      val newBytes = new Array[Byte](newSize)
      System.arraycopy(bytes, 0, newBytes, 0, length)
      bytes = newBytes
    }
  }

  def writeByte(b: Int): Unit = {
    ensureCapacity(1)
    bytes(length) = b.toByte
    length += 1
  }

  def writeNat(x: Int): Unit = {
    var value = x
    while (value > 127) {
      writeByte(value & 0x7F)
      value >>>= 7
    }
    writeByte(value | 0x80)
  }

  def writeInt(x: Int): Unit = {
    var value = x
    val negative = value < 0
    if (negative) value = ~value

    var more = true
    while (more) {
      var byte = value & 0x3F
      value >>>= 6
      if (value != 0 || negative) byte |= 0x40
      if (value != 0) byte &= ~0x80
      else more = false
      writeByte(byte | 0x80)
    }
  }

  def writeLongInt(x: Long): Unit = {
    var value = x
    val negative = value < 0
    if (negative) value = ~value

    var more = true
    while (more) {
      var byte = (value & 0x3F).toInt
      value >>>= 6
      if (value != 0 || negative) byte |= 0x40
      if (value != 0) byte &= ~0x80
      else more = false
      writeByte(byte | 0x80)
    }
  }
}

