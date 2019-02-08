package dotty.tools
package backend
package jvm

import scala.tools.asm

object GenBCodeOps extends GenBCodeOps

class GenBCodeOps {
  def mkFlags(args: Int*) = args.foldLeft(0)(_ | _)

  final val PublicStatic      = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC
  final val PublicStaticFinal = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL
}
