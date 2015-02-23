package dotty.tools.dotc
package core
package pickling

import Contexts._, Decorators._
import printing.Texts._
import TastyName._
import TastyUnpickler._

class TastyPrinter(bytes: Array[Byte])(implicit ctx: Context) {
  
  val unpickler = new TastyUnpickler(bytes)
  import unpickler.{tastyName, unpickle}
  
  def nameToString(name: TastyName): String = name match {
    case Simple(name) => name.toString
    case Qualified(qual, name) => nameRefToString(qual) + "." + nameRefToString(name)
    case Signed(original, params, result) => 
      i"${nameRefToString(original)}@${params.map(nameRefToString)}%,%:${nameRefToString(result)}"
    case Expanded(original) => nameRefToString(original) + "/EXPANDED"
    case ModuleClass(original) => nameRefToString(original) + "/MODULECLASS"
    case SuperAccessor(accessed) => nameRefToString(accessed) + "/SUPERACCESSOR"
    case DefaultGetter(meth, num) => nameRefToString(meth) + "/DEFAULTGETTER" + num
  }
      
  def nameRefToString(ref: NameRef): String = nameToString(tastyName(ref))
  
  def printNames() = 
    for ((name, idx) <- tastyName.contents.zipWithIndex)
      println(f"$idx%4d: " + nameToString(name))
  
  def printContents(): Unit = {
    println("Names:")
    printNames()
    println("Trees:")
    unpickle(new TreeUnpickler)
    unpickle(new PositionUnpickler)
  }
  
  class TreeUnpickler extends SectionUnpickler[Unit]("ASTs") {
    import PickleFormat._
    def unpickle(reader: TastyReader, tastyName: TastyName.Table): Unit = {
      import reader._
      var indent = 0
      def newLine() = print(f"\n ${index(currentAddr) - index(startAddr)}%5d:" + " " * indent)
      def printNat() = print(" " + readNat())
      def printName() = {
        val idx = readNat()
        print(" ") ;print(idx); print("["); print(nameRefToString(NameRef(idx))); print("]")
      }
      def printTree(): Unit = {
        newLine()
        val tag = readByte()
        print(" ");print(astTagToString(tag))
        indent += 2
        if (tag >= firstLengthTreeTag) {
          val len = readNat()
          print(s"($len)")
          val end = currentAddr + len
          def printTrees() = until(end)(printTree())
          tag match {
            case IMPORTED => 
              printName()
            case RENAMED =>
              printName(); printName()
            case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | SELFDEF | BIND | REFINEDtype => 
              printName(); printTrees()
            case RETURN =>
              printNat(); printTrees() 
            case METHODtype | POLYtype =>
              printTree()
              until(end) { printName(); printTree() }
            case PARAMtype =>
              printNat(); printNat()
            case _ =>
              printTrees()
          }
          if (currentAddr != end) {
            println(s"incomplete read, current = $currentAddr, end = $end")
            skipTo(currentAddr)
          }
        }
        else if (tag >= firstNatASTTreeTag) { 
          tag match {
            case IDENT | SELECT | TERMREF | TYPEREF => printName()
            case _ => printNat() 
          }
          printTree()
        }
        else if (tag >= firstNatTreeTag) 
          tag match {
            case TERMREFstatic | TYPEREFstatic | STRINGconst => printName()
            case _ => printNat()
          }
        indent -= 2
      }
      println(i"start = ${reader.startAddr}, base = $base, current = $currentAddr, end = $endAddr")
      println(s"${endAddr.index - startAddr.index} bytes of AST, base = $currentAddr")
      while (!isAtEnd) {
        printTree()
        newLine()
      }
    }
  }
  
  class PositionUnpickler extends SectionUnpickler[Unit]("Positions") {
    def unpickle(reader: TastyReader, tastyName: TastyName.Table): Unit = {
      import reader._

      def unpickleOffsets(edge: Edge): Unit = {
        var lastOffset = 0
        var lastAddr = 0
        val length = readNat()
        println(s"$length offset bytes")
        val end = currentAddr + length
        until(end) {
          lastOffset += readInt()
          lastAddr += readInt()
          println(s"$lastOffset: $lastAddr")
        }
      }
      
      unpickleOffsets(Edge.left)
      unpickleOffsets(Edge.right)
    }
  }
}