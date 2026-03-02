package dotty.tools
package backend
package jvm

import scala.tools.asm

object GenBCodeOps {
  extension (flags: Int)
    def addFlagIf(cond: Boolean, flag: Int): Int = if cond then flags | flag else flags

  final val PublicStatic      = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC
  final val PublicStaticFinal = asm.Opcodes.ACC_PUBLIC | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL
  final val PrivateStaticFinal = asm.Opcodes.ACC_PRIVATE | asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL
}
