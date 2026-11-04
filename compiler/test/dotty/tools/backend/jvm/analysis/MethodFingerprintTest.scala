package dotty.tools.backend.jvm.analysis

import org.junit.Test
import org.junit.Assert.*
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.tree.*

class MethodFingerprintTest {
  // static int f(int x) { L0: if (x == 0) goto L1; return 1; L1: return 0; L2: }, with a try-catch block and a local variable
  final class Fixture {
    val l0, l1, l2 = new LabelNode
    val load = new VarInsnNode(ILOAD, 0)
    val jump = new JumpInsnNode(IFEQ, l1)
    val call = new MethodInsnNode(INVOKESTATIC, "C", "g", "()V", false)
    val tcb = new TryCatchBlockNode(l0, l1, l1, "java/lang/Exception")
    val lv = new LocalVariableNode("x", "I", null, l0, l2, 0)
    val method = new MethodNode(ACC_PUBLIC | ACC_STATIC, "f", "(I)I", null, null)
    for (i <- List(l0, load, jump, call, new InsnNode(ICONST_1), new InsnNode(IRETURN), l1, new InsnNode(ICONST_0), new InsnNode(IRETURN), l2))
      method.instructions.add(i)
    method.tryCatchBlocks.add(tcb)
    method.localVariables.add(lv)
    method.maxLocals = 1
    method.maxStack = 1
  }

  val edits: List[(String, Fixture => Unit)] = List(
    "insert instruction" -> (f => f.method.instructions.insert(new InsnNode(NOP))),
    "remove instruction" -> (f => f.method.instructions.remove(f.call)),
    "replace instruction" -> (f => f.method.instructions.set(f.load, new VarInsnNode(ILOAD, 0))),
    "opcode" -> (f => f.load.setOpcode(FLOAD)),
    "local variable operand" -> (f => f.load.`var` = 1),
    "jump target" -> (f => f.jump.label = f.l0),
    "call target" -> (f => f.call.name = "h"),
    "try-catch range" -> (f => f.tcb.end = f.l0),
    "try-catch type" -> (f => f.tcb.`type` = "java/lang/Throwable"),
    "max locals" -> (f => f.method.maxLocals += 1),
    "max stack" -> (f => f.method.maxStack += 1),
    "local variable entry" -> (f => f.lv.desc = "J"),
  )

  @Test def unchangedMethodHasSameFingerprint(): Unit = {
    val f = new Fixture
    assertEquals(MethodFingerprint(f.method), MethodFingerprint(f.method))
  }

  @Test def editsChangeFingerprint(): Unit = {
    val undetected = edits.filter { (_, edit) =>
      val f = new Fixture
      val before = MethodFingerprint(f.method)
      edit(f)
      MethodFingerprint(f.method) == before
    }
    assertTrue(s"edits not reflected in the fingerprint: ${undetected.map(_._1)}", undetected.isEmpty)
  }

  @Test def neverTheSentinel(): Unit =
    assertNotEquals(MethodFingerprint.NoFingerprint, MethodFingerprint(new Fixture().method))
}
