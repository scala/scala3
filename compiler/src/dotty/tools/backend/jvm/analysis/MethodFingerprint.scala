package dotty.tools
package backend.jvm
package analysis

import org.objectweb.asm.tree.*

/**
 * Identifies the code of a method at some point, to check that it has not been changed in between two
 * points. It covers the instruction list (node identities and order, opcodes and operands), the try-catch
 * blocks, the local variable table, and `maxLocals` / `maxStack`; so it changes whether instructions are
 * added, removed or replaced, or edited in place. Computed in one pass over the method.
 */
private[analysis] final case class MethodFingerprint private (hash: Long)

private[analysis] object MethodFingerprint {
  /** Not the fingerprint of any method. */
  val NoFingerprint: MethodFingerprint = new MethodFingerprint(0L)

  def apply(method: MethodNode): MethodFingerprint = {
    var h = 17L
    def add(x: Long): Unit = h = h * 31 + x
    def addRef(ref: AnyRef | Null): Unit = add(System.identityHashCode(ref))
    def addVal(v: Any): Unit = add(v.##)

    add(method.maxLocals)
    add(method.maxStack)

    var insn = method.instructions.getFirst
    while (insn != null) {
      addRef(insn)
      add(insn.getOpcode)
      insn match {
        case i: VarInsnNode => add(i.`var`)
        case i: IincInsnNode => add(i.`var`); add(i.incr)
        case i: IntInsnNode => add(i.operand)
        case i: JumpInsnNode => addRef(i.label)
        case i: TableSwitchInsnNode => add(i.min); add(i.max); addRef(i.dflt); i.labels.forEach(addRef)
        case i: LookupSwitchInsnNode => addRef(i.dflt); i.keys.forEach(k => addVal(k)); i.labels.forEach(addRef)
        case i: MethodInsnNode => addVal(i.owner); addVal(i.name); addVal(i.desc); addVal(i.itf)
        case i: FieldInsnNode => addVal(i.owner); addVal(i.name); addVal(i.desc)
        case i: TypeInsnNode => addVal(i.desc)
        case i: LdcInsnNode => addVal(i.cst)
        case i: MultiANewArrayInsnNode => addVal(i.desc); add(i.dims)
        case i: InvokeDynamicInsnNode => addVal(i.name); addVal(i.desc); addVal(i.bsm); addVal(java.util.Arrays.asList(i.bsmArgs*))
        case i: LineNumberNode => add(i.line); addRef(i.start)
        case _ =>
      }
      insn = insn.getNext
    }

    method.tryCatchBlocks.forEach { t =>
      addRef(t.start); addRef(t.end); addRef(t.handler); addVal(t.`type`)
    }
    if (method.localVariables != null) method.localVariables.forEach { v =>
      add(v.index); addVal(v.name); addVal(v.desc); addVal(v.signature); addRef(v.start); addRef(v.end)
    }

    // keep clear of the sentinel
    new MethodFingerprint(if (h == NoFingerprint.hash) 1L else h)
  }
}
