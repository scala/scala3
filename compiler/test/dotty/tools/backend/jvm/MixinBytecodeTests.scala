package dotty.tools.backend.jvm

import dotty.tools.io.AbstractFile

import org.junit.Assert.*
import org.junit.Test

import scala.jdk.CollectionConverters.*
import scala.tools.asm.Opcodes.*
import scala.tools.asm.tree.ClassNode
import dotty.tools.backend.jvm.ASMConverters.*

class MixinBytecodeTestsNoForwarders extends DottyBytecodeTest {
  override def initCtx = {
    val ctx0 = super.initCtx
    ctx0.setSetting(ctx0.settings.XmixinForceForwarders, "false")
  }

  private def getClass(dir: AbstractFile, name: String) =
    loadClassNode(dir.lookupName(name + ".class", directory = false).input)

  private def checkForwarder(clazz: ClassNode, target: String) = {
    val f = getMethod(clazz, "f")
    assertSameCode(f, List(VarOp(ALOAD, 0), Invoke(INVOKESTATIC, target, "f$", s"(L$target;)I", itf = true), Op(IRETURN)))
  }

  @Test
  def t10853(): Unit = {
    val code =
      """trait F[T1, R] { def apply(funArg: T1): R }
        |
        |trait SetOps[A, +C] extends F[A, Boolean] { final def apply(setEl: A): Boolean = false }
        |
        |class AbstractSet[A] extends SetOps[A, AbstractSet[A]]
        |class AbstractSet2[A] extends AbstractSet[A] with SetOps[A, AbstractSet2[A]]
      """.stripMargin

    checkBCode(code) { dir =>
      val clsIn = dir.lookupName("AbstractSet2.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      assertEquals(clsNode.methods.asScala.map(_.name).toList, List("<init>")) // no bridge for apply (there's already one in AbstractSet)
    }
  }

  @Test
  def traitMethodForwarders(): Unit = {
    val code =
      """trait T1 { def f = 1 }
        |trait T2 extends T1 { override def f = 2 }
        |trait T3 { self: T1 => override def f = 3 }
        |
        |abstract class A1 { def f: Int }
        |class A2 { def f: Int = 4 }
        |
        |trait T4 extends A1 { def f = 5 }
        |trait T5 extends A2 { override def f = 6 }
        |
        |trait T6 { def f: Int }
        |trait T7 extends T6 { abstract override def f = super.f + 1 }
        |
        |trait T8 { override def clone() = super.clone() }
        |
        |class A3 extends T1 { override def f = 7 }
        |
        |class C1 extends T1
        |class C2 extends T2
        |class C3 extends T1 with T2
        |class C4 extends T2 with T1
        |class C5 extends T1 with T3
        |
        |// traits extending a class that defines f
        |class C6 extends T4
        |class C7 extends T5
        |class C8 extends A1 with T4
        |class C9 extends A2 with T5
        |
        |// T6: abstract f in trait
        |class C10 extends T6 with T1
        |class C11 extends T6 with T2
        |abstract class C12 extends A1 with T6
        |class C13 extends A2 with T6
        |class C14 extends T4 with T6
        |class C15 extends T5 with T6
        |
        |// superclass overrides a trait method
        |class C16 extends A3
        |class C17 extends A3 with T1
        |
        |// abstract override
        |class C18 extends T6 { def f = 22 }
        |class C19 extends C18 with T7
        |
        |class C20 extends T8
      """.stripMargin

    checkBCode(code) { dir =>
      val noForwarder = List("C1", "C2", "C3", "C4", "C10", "C11", "C12", "C13", "C16", "C17")
      val noForwarderClasses = noForwarder.map(cn => loadClassNode(dir.lookupName(cn + ".class", directory = false).input))
      for cn <- noForwarderClasses do
        val meth = cn.methods.asScala.find(_.name == "f")
        assert(meth.isEmpty, s"failed for ${cn.name}")

      checkForwarder(getClass(dir, "C5"), "T3")
      checkForwarder(getClass(dir, "C6"), "T4")
      checkForwarder(getClass(dir, "C7"), "T5")
      checkForwarder(getClass(dir, "C8"), "T4")
      checkForwarder(getClass(dir, "C9"), "T5")
      checkForwarder(getClass(dir, "C14"), "T4")
      checkForwarder(getClass(dir, "C15"), "T5")
      assertSameCode(getMethod(getClass(dir, "C18"), "f"), List(IntOp(BIPUSH, 22), Op(IRETURN)))
      checkForwarder(getClass(dir, "C19"), "T7")
      assertSameCode(getMethod(getClass(dir, "C19"), "T7$$super$f"), List(VarOp(ALOAD, 0), Invoke(INVOKESPECIAL, "C18", "f", "()I", itf = false), Op(IRETURN)))
      assertInvoke(getMethod(getClass(dir, "C20"), "clone"), "T8", "clone$") // mixin forwarder
    }
  }

  @Test
  def noTraitMethodForwardersForOverloads(): Unit = {
    val code =
      """trait T1 { def f(x: Int) = 0 }
        |trait T2 { def f(x: String) = 1 }
        |class C extends T1 with T2
      """.stripMargin


    checkBCode(code) { dir =>
      assert(!getClass(dir, "C").methods.asScala.exists(_.name == "f"))
    }
  }

  @Test
  def traitMethodForwardersForJavaDefaultMethods(): Unit = {
    val j1 = "interface J1 { int f(); }"
    val j2 = "interface J2 { default int f() { return 1; } }"
    val j3 = "interface J3 extends J1 { default int f() { return 2; } }"
    val j4 = "interface J4 extends J2 { default int f() { return 3; } }"
    val code =
      """trait T1 extends J2 { override def f = 4 }
        |trait T2 { self: J2 => override def f = 5 }
        |
        |class K1 extends J2
        |class K2 extends J1 with J2
        |class K3 extends J2 with J1
        |
        |class K4 extends J3
        |class K5 extends J3 with J1
        |class K6 extends J1 with J3
        |
        |class K7 extends J4
        |class K8 extends J4 with J2
        |class K9 extends J2 with J4
        |
        |class K10 extends T1 with J2
        |class K11 extends J2 with T1
        |
        |class K12 extends J2 with T2
      """.stripMargin

    checkBCode(scalaSources = List(code), javaSources = List(j1, j2, j3, j4)) { dir =>
      val noForwarder = List("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10", "K11")
      for (cn <- noForwarder)
        assert(!getClass(dir, cn).methods.asScala.exists(_.name == "f"))
      checkForwarder(getClass(dir, "K12"), "T2")
    }
  }

  @Test
  def mixinForwarders(): Unit = {
    val code =
      """trait T { def f = 1 }
        |class C extends T
      """.stripMargin
    checkBCode(code) { dir =>
      assert(!getClass(dir, "C").methods.asScala.exists(_.name == "f"))
    }
  }

  @Test
  def sd143a(): Unit = {
    val code =
      """trait A { def m = 1 }
        |class B extends A { override def m = 2 }
        |trait T extends A
        |class C extends B with T {
        |  override def m = super[T].m
        |}
      """.stripMargin

    checkBCode(code) { dir =>
      val cls = getClass(dir, "C")
      val instrs = getInstructions(cls, "m")
      assert(instrs.contains(Invoke(INVOKESTATIC, "A", "m$", "(LA;)I", itf = true)), instrs.mkString("\n"))
    }
  }

  @Test
  def sd143b(): Unit = {
    val jCode = "interface A { default int m() { return 1; } }"
    val code =
      """class B extends A { override def m = 2 }
        |trait T extends A
        |class C extends B with T {
        |  override def m = super[T].m
        |}
      """.stripMargin

    checkBCode(scalaSources = List(code), javaSources = List(jCode)) { dir =>
      val cls = getClass(dir, "C")
      val instrs = getInstructions(cls, "m")
      // REVIEW: scala2 called T.m here, we call A.m, but it should be the same? invokespecial only requires direct parents for interfaces
      assert(instrs.contains(Invoke(INVOKESPECIAL, "A", "m", "()I", itf = true)), instrs.mkString("\n"))
    }
  }

  @Test
  def sd143c(): Unit = {
    // Allow super calls to class methods of indirect super classes
    val code =
      """class A { def f = 1 }
        |class B extends A
        |trait T extends A { override def f = 2 }
        |class C extends B with T {
        |  def t1 = super[B].f
        |  def t2 = super.f
        |  def t3 = super[T].f
        |}
      """.stripMargin

    checkBCode(code) { dir =>
      val c = getClass(dir, "C")
      val t1 = getInstructions(c, "t1")
      // REVIEW: scala2 called A.f here, we call B.f, but this seems simpler since A is not a direct parent of C
      assert(t1.contains(Invoke(INVOKESPECIAL, "B", "f", "()I", itf = false)), t1.mkString("\n"))
      val t2 = getInstructions(c, "t2")
      val invStat = Invoke(INVOKESTATIC, "T", "f$", "(LT;)I", itf = true)
      assert(t2.contains(invStat), t2.mkString("\n"))
      val t3 = getInstructions(c, "t3")
      assert(t3.contains(invStat), t3.mkString("\n"))
    }
  }

  @Test
  def sd143d(): Unit = {
    val jCode = "interface T { default int f() { return 1; } }"
    val code =
      """trait U1 extends T
        |trait U2 extends T
        |class C extends U1 with U2 { def t = super.f }
      """.stripMargin
    checkBCode(scalaSources = List(code), javaSources = List(jCode)) { dir =>
      val c = getClass(dir, "C")
      val t = getInstructions(c, "t")
      // REVIEW: the following comment is from scala2 where U1.f was selected, presumably T.f is better?
      // super call to T.f in C is allowed even if T is not a direct parent, the compiler
      // picks U1 as receiver in the invokespecial descriptor.
      assert(t.contains(Invoke(INVOKESPECIAL, "T", "f", "()I", itf = true)), t.mkString("\n"))
    }
  }

  @Test
  def sd210(): Unit = {
    val jCode = "interface A { default int m() { return 1; } }"
    // used to crash in the backend (scala/scala-dev#210) under `-Xmixin-force-forwarders:true`
    val code1 =
      """trait B1 extends A // called "B1" not "B" due to scala-dev#214
        |class C extends B1
      """.stripMargin
    checkBCode(scalaSources = List(code1), javaSources = List(jCode)) { dir =>
      assert(!getClass(dir, "C").methods.asScala.exists(_.name == "m")) // ok, no forwarder
    }

    val code3 =
      """abstract class B { def m: Int }
        |class C extends B with A
      """.stripMargin
    checkBCode(scalaSources = List(code3), javaSources = List(jCode)) { dir =>
      // invokespecial to A.m is correct here: A is an interface, so resolution starts at A.
      // https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.invokespecial
      val ins3 = getInstructions(getClass(dir, "C"), "m")
      assert(ins3.contains(Invoke(INVOKESPECIAL, "A", "m", "()I", itf = true)), ins3.mkString("\n"))
    }

    val code4 =
      """trait B { self: A => override def m = 2 }
        |class C extends A with B // forwarder, invokestatic B.m$
      """.stripMargin
    checkBCode(scalaSources = List(code4), javaSources = List(jCode)) { dir =>
      val ins4 = getInstructions(getClass(dir, "C"), "m")
      assert(ins4.contains(Invoke(INVOKESTATIC, "B", "m$", "(LB;)I", itf = true)), ins4.mkString("\n"))
    }

    // scala-only example
    val code5 =
      """trait AS { def m = 1 }
        |abstract class B { def m: Int }
        |class C extends B with AS // forwarder, invokestatic AS.m$
      """.stripMargin
    checkBCode(code5) { dir =>
      val ins5 = getInstructions(getClass(dir, "C"), "m")
      assert(ins5.contains(Invoke(INVOKESTATIC, "AS", "m$", "(LAS;)I", itf = true)), ins5.mkString("\n"))
    }
  }
}

class MixinBytecodeTestsWithForwarders extends DottyBytecodeTest {
  private def getClass(dir: AbstractFile, name: String) =
    loadClassNode(dir.lookupName(name + ".class", directory = false).input)

  @Test
  def sd224(): Unit = {
    val jCode = "interface T { default int f() { return 1; } }"
    val code =
      """trait U extends T
        |class C extends U { def t = super.f }
      """.stripMargin
    checkBCode(scalaSources = List(code), javaSources = List(jCode)) { dir =>
      val ins = getInstructions(getClass(dir, "C"), "t")
      // REVIEW: scala2 called U.f here, but this should be fine? T is an interface so no need to be a direct parent for invokespecial
      assert(ins.contains(Invoke(INVOKESPECIAL, "T", "f", "()I", itf = true)), ins.mkString("\n"))
    }
  }

  @Test
  def noMinimizeJavaInterfaces(): Unit = {
    val jCode = "interface T { default int f() { return 1; } }"
    val code =
      """trait U extends T { override def f() = 2 }
        |class C extends T with U { def t = super[T].f }
      """.stripMargin
    checkBCode(scalaSources = List(code), javaSources = List(jCode)) { dir =>
      val c = getClass(dir, "C")
      assert(c.interfaces.asScala.toList.sorted == List("T", "U"), c.interfaces.asScala.mkString(", "))
      val ins = getInstructions(c, "t")
      assert(ins.contains(Invoke(INVOKESPECIAL, "T", "f", "()I", itf = true)), ins.mkString("\n"))
    }
  }

  @Test
  def noMinimizeScalaTraitAccessingJavaMember(): Unit = {
    val jCode = "interface A { default int f() { return 1; } }"
    val code =
      """trait U extends A
        |trait V extends U
        |class C extends U with V { def t = super.f() }
      """.stripMargin
    checkBCode(scalaSources = List(code), javaSources = List(jCode)) { dir =>
      val c = getClass(dir, "C")
      val ins = getInstructions(c, "t")
      // REVIEW: in scala2 both instances of "A" were "U" instead. But we don't need U at all, so this should be ok?
      assert(c.interfaces.asScala.toList.sorted == List("A", "V"), c.interfaces.asScala.mkString(", "))
      assert(ins.contains(Invoke(INVOKESPECIAL, "A", "f", "()I", itf = true)), ins.mkString("\n"))
    }
  }

  def invSt(instrs: List[Instruction], receiver: String, method: String = "f$", itf: Boolean = true): Unit =
    assert(instrs.contains(Invoke(INVOKESTATIC, receiver, method, s"(L$receiver;)I", itf)), instrs.mkString("\n"))
  def invSp(instrs: List[Instruction], receiver: String, method: String = "f", sig: String = "()I", itf: Boolean = true): Unit =
    assert(instrs.contains(Invoke(INVOKESPECIAL, receiver, method, sig, itf)), instrs.mkString("\n"))

  @Test
  def superCalls1(): Unit = {
    val code =
      """trait T { def f = 1 }
        |trait U extends T
        |class C extends U { def t = super.f }
      """.stripMargin
    checkBCode(code) { dir =>
      val c = getClass(dir, "C")
      assert(c.interfaces.asScala.toList == List("U"), c.interfaces.asScala.mkString(", "))
      invSt(getInstructions(c, "t"), "T")
      invSt(getInstructions(c, "f"), "T")
    }
  }

  @Test
  def superCalls2(): Unit = {
    val code =
      """class A { def f = 1 }
        |trait T extends A { override def f = 2 }
        |class B extends A
        |class C extends B with T {
        |  def t1 = super.f
        |  def t2 = super[T].f
        |  def t3 = super[B].f
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = getClass(dir, "C")
      invSt(getInstructions(c, "f"), "T")
      invSt(getInstructions(c, "t1"), "T")
      invSt(getInstructions(c, "t2"), "T")
      // REVIEW: scala2 called A here
      invSp(getInstructions(c, "t3"), "B", itf = false)
    }
  }

  @Test
  def superCalls3(): Unit = {
    val code =
      """class A { def f = 1 }
        |trait T extends A
        |class B extends A { override def f = 2 }
        |class C extends B with T {
        |  def t1 = super.f
        |  // def t2 = super[T].f // error: cannot emit super call. tested in sd143
        |  def t3 = super[B].f
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = getClass(dir, "C")
      invSp(getInstructions(c, "t1"), "B", itf = false)
      invSp(getInstructions(c, "t3"), "B", itf = false)
      assert(!c.methods.asScala.exists(_.name == "f"))
    }
  }

  @Test
  def superCalls4(): Unit = {
    val code =
      """trait T1 { def f = 1 }
        |trait T2 { self: T1 => override def f = 2 }
        |trait U extends T1 with T2
        |class C extends U {
        |  def t1 = super.f
        |  def t2 = super[U].f
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = getClass(dir, "C")
      assert(c.interfaces.asScala.toList == List("U"), c.interfaces.asScala.mkString(", "))
      invSt(getInstructions(c, "f"), "T2")
      invSt(getInstructions(c, "t1"), "T2")
      invSt(getInstructions(c, "t2"), "T2")
    }
  }

  @Test
  def superCalls5(): Unit = {
    val code =
      """trait T1 { def f = 1 }
        |trait T2 { self: T1 => override def f = 2 }
        |trait U extends T1 with T2
        |class C extends U with T1 with T2
      """.stripMargin
    checkBCode(code) { dir =>
      val c = getClass(dir, "C")
      // T1, T2 removed by minimizeParents
      assert(c.interfaces.asScala.toList == List("U"), c.interfaces.asScala.mkString(", "))
      invSt(getInstructions(c, "f"), "T2")
    }
  }

  @Test
  def superCalls6(): Unit = {
    val code =
      """trait T { override def hashCode = -1 }
        |trait U extends T
        |class C extends U {
        |  def t1 = super[U].hashCode
        |  def t2 = super.hashCode
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = getClass(dir, "C")
      assert(c.interfaces.asScala.toList == List("U"), c.interfaces.asScala.mkString(", "))
      invSt(getInstructions(c, "hashCode"), "T", "hashCode$")
      invSt(getInstructions(c, "t1"), "T", "hashCode$")
      invSt(getInstructions(c, "t2"), "T", "hashCode$")
    }
  }

  @Test
  def superCalls7(): Unit = {
    val code =
      """trait T { def f = 1 }
        |trait U1 extends T { override def f = 2 }
        |trait U2 extends T { override def f = 3 }
        |class C1 extends T with U1 with U2 {
        |  def t1 = super.f
        |  def t2 = super[T].f
        |  def t3 = super[U1].f
        |  def t4 = super[U2].f
        |}
        |class C2 extends T with U2 with U1 {
        |  def t1 = super.f
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c1 = getClass(dir, "C1")
      val c2 = getClass(dir, "C2")
      assert(c1.interfaces.asScala.sorted.toList == List("U1", "U2"), c1.interfaces.asScala.mkString(", "))
      assert(c2.interfaces.asScala.sorted.toList == List("U1", "U2"), c2.interfaces.asScala.mkString(", "))
      invSt(getInstructions(c1, "f"), "U2")
      invSt(getInstructions(c1, "t1"), "U2")
      invSt(getInstructions(c1, "t2"), "T")
      invSt(getInstructions(c1, "t3"), "U1")
      invSt(getInstructions(c1, "t4"), "U2")
      invSt(getInstructions(c2, "f"), "U1")
      invSt(getInstructions(c2, "t1"), "U1")
    }
  }

  @Test
  def superCalls8(): Unit = {
    val code =
      """trait T1 { def f = 1 }
        |trait T2 { self: T1 => override def f = 2 }
        |trait U extends T1 with T2
        |trait V extends U with T2
        |class C extends V {
        |  def t1 = super.f
        |  def t2 = super[V].f
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = getClass(dir, "C")
      assert(c.interfaces.asScala.toList == List("V"), c.interfaces.asScala.mkString(", "))
      invSt(getInstructions(c, "f"), "T2")
      invSt(getInstructions(c, "t1"), "T2")
      invSt(getInstructions(c, "t2"), "T2")
    }
  }

  @Test
  def superCalls9(): Unit = {
    val code =
      """trait T { def f: Int }
        |trait U1 extends T { def f = 0 }
        |trait U2 extends T { override def f = 1 }
        |trait V extends U1
        |
        |trait W1 extends V with U2
        |class C1 extends W1 with U2
        |
        |trait W2 extends V with U2 { override def f = super[U2].f }
        |class C2 extends W2 with U2
        |
        |trait W3 extends V with U2 { override def f = super.f }
        |class C3 extends W3 with U2
      """.stripMargin
    checkBCode(code) { dir =>
      val c1 = getClass(dir, "C1")
      val c2 = getClass(dir, "C2")
      val c3 = getClass(dir, "C3")

      assert(c1.interfaces.asScala.toList == List("W1"), c1.interfaces.asScala.mkString(", "))
      invSt(getInstructions(c1, "f"), "U2")

      assert(c2.interfaces.asScala.toList == List("W2"), c1.interfaces.asScala.mkString(", "))
      invSt(getInstructions(c2, "f"), "W2")

      assert(c3.interfaces.asScala.toList == List("W3"), c3.interfaces.asScala.mkString(", "))
      invSt(getInstructions(c3, "W3$$super$f"), "U2")
      invSt(getInstructions(c3, "f"), "W3")
    }
  }

  @Test
  def superReceiver(): Unit = {
    val code =
      """trait A {
        |  def m = 1
        |}
        |trait B extends A
        |class SK
        |class C extends SK with B {
        |  override def m = super.m + 1
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = getClass(dir, "C")
      assertInvoke(getInstructions(c, "m"), "A", "m$")
    }
  }
}