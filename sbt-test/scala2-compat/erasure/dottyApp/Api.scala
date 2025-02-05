// Keep synchronized with scala2Lib/Api.scala
package dottyApp

class foo extends scala.annotation.StaticAnnotation

trait A
trait B
trait SubB extends B
trait C
trait Cov[+T]
trait Univ extends Any

class D

class VC(val self: A) extends AnyVal

class Outer {
  class E
  trait F extends E
}

// The parameter type of `a_XX` should erase to A, `b_XX` to `B`, etc.
// This is enforced by dottyApp/Main.scala
class Z {
  def a_01(a: A with B): Unit = {}
  def a_02X(b: B with A): Unit = {}
  def a_02(a: A with B with A): Unit = {}
  def a_03(a: A with (B with A)): Unit = {}
  def a_04(b: A with (B with A) @foo): Unit = {}
  def a_04X(b: A with (B with C) @foo): Unit = {}
  def a_05(b: A with (B with A) @foo with (C with B with A) @foo): Unit = {}

  type T1 <: A with B
  def a_06(a: T1): Unit = {}

  type S <: B with T1
  def a_07(a: S): Unit = {}

  type T2 <: B with A
  type U <: T2 with S
  def a_08(b: U): Unit = {}

  val singB: B = new B {}
  def a_09(a: A with singB.type): Unit = {}
  def a_10(b: singB.type with A): Unit = {}

  type V >: SubB <: B
  def b_11(b: V): Unit = {}
  def subb_12(b: V with SubB): Unit = {}

  def d_13(d: D with A): Unit = {}
  def d_14(d: A with D): Unit = {}

  val singD: D = new D {}
  def d_13x(d: singD.type with A): Unit = {}
  def d_14x(d: A with singD.type): Unit = {}

  type DEq = D
  def d_15(d: A with DEq): Unit = {}
  def d_16(d: A with (DEq @foo)): Unit = {}
  def d_17(d: DEq with A): Unit = {}
  def d_18(d: (DEq @foo) with A): Unit = {}

  val singDEq: DEq @foo = new D {}
  def d_15b(d: A with singDEq.type): Unit = {}
  def d_16b(d: A with (singDEq.type @foo)): Unit = {}

  type DSub <: D
  def d_19(a: A with DSub): Unit = {}
  def d_19x(d: DSub with A): Unit = {}
  def d_20(z: DSub with Z): Unit = {}

  type W1 <: A with Cov[Any]
  type X1 <: Cov[Int] with W1
  def a_21(a: X1): Unit = {}

  type W2 <: A with Cov[Any]
  type X2 <: Cov[Int] with W2
  def a_22(a: X2): Unit = {}

  def z_23(z: A with this.type): Unit = {}
  def z_24(z: this.type with A): Unit = {}

  def a_25(b: A with (B { type T })): Unit = {}
  def a_26(a: (A { type T }) with ((B with A) { type T })): Unit = {}

  def a_27(a: VC with B): Unit = {}
  def a_28(a: B with VC): Unit = {}

  val o1: Outer = new Outer
  val o2: Outer = new Outer
  def e_29(f: o1.E with o1.F): Unit = {}
  def e_30(f: o1.F with o1.E): Unit = {}
  def e_31(f: o1.E with o2.F): Unit = {}
  def e_32(f: o2.F with o1.E): Unit = {}
  def e_33(f: Outer#E with Outer#F): Unit = {}
  def e_34(f: Outer#F with Outer#E): Unit = {}

  val structural1: { type DSub <: D } = new { type DSub <: D }
  def d_35(a: A with structural1.DSub): Unit = {}
  def d_36(a: structural1.DSub with A): Unit = {}
  def d_37(z: Z with structural1.DSub): Unit = {}
  def d_38(z: structural1.DSub with Z): Unit = {}

  val structural2: { type SubCB <: C with B } = new { type SubCB <: C with B }
  def b_39(c: structural2.SubCB with B): Unit = {}
  def b_40(c: B with structural2.SubCB): Unit = {}

  val structural3a: { type SubB <: B; type SubCB <: C with SubB } = new { type SubB <: B; type SubCB <: C with SubB }
  val structural3b: { type SubB <: B; type SubCB <: C with SubB } = new { type SubB <: B; type SubCB <: C with SubB }
  def b_41(c: structural3a.SubB with structural3a.SubCB): Unit = {}
  def b_42(c: structural3a.SubCB with structural3a.SubB): Unit = {}
  def b_43(b: structural3a.SubB with structural3b.SubCB): Unit = {}
  def b_44(c: structural3b.SubCB with structural3a.SubB): Unit = {}

  type SubStructural <: C with structural3a.SubB
  def b_45(x: structural3a.SubB with SubStructural): Unit = {}
  def b_46(x: structural3b.SubB with SubStructural): Unit = {}

  type Rec1 <: A with B
  type Rec2 <: C with Rec1
  def a_47(a: A with B with Rec2): Unit = {}
  def a_48(a: (A with B) @foo with Rec2): Unit = {}

  type F1 = A with B
  type F2 = A with B
  type Rec3 <: F1
  type Rec4 <: C with Rec3
  def a_49(a: F1 @foo with Rec4): Unit = {}
  def a_50(a: F1 with Rec4): Unit = {}
  def a_51(a: F2 @foo with Rec4): Unit = {}
  def a_52(a: F2 with Rec4): Unit = {}

  type AA = A
  type F3 = AA with B
  type Rec5 <: F3
  type Rec6 <: C with Rec5
  def a_53(a: F3 @foo with Rec6): Unit = {}
  def a_54(a: F3 with Rec6): Unit = {}

  val structural4a: { type M[X] <: A } = new { type M[X] <: A }
  val structural4b: { type N <: B with structural4a.M[Int] } = new { type N <: B with structural4a.M[Int] }
  def a_55(x: structural4a.M[Any] with structural4b.N): Unit = {}

  type Bla = A { type M[X] <: A }
  def a_56(x: Bla#M[Any] with ({ type N <: B with Bla#M[Int] })#N): Unit = {}
  type AEq = A
  type Bla2 = AEq { type M[X] <: A }
  def a_57(x: Bla2#M[Any] with ({ type N <: B with Bla2#M[Int] })#N): Unit = {}

  def int_58(x: Int with Singleton): Unit = {}
  def int_59(x: Singleton with Int): Unit = {}
  def int_60(x: Int with Any): Unit = {}
  def int_61(x: Any with Int): Unit = {}
  def int_62(x: Int with AnyVal): Unit = {}
  def int_63(x: AnyVal with Int): Unit = {}

  def intARRAY_64(x: Array[Int with Singleton]): Unit = {}
  def intARRAY_65(x: Array[_ <: Int]): Unit = {}
  def intARRAY_66(x: Array[_ <: Int with Singleton]): Unit = {}
  def intARRAY_67(x: Array[_ <: Singleton with Int]): Unit = {}
  def intARRAY_68(x: Array[_ <: Int with Any]): Unit = {}
  def intARRAY_69(x: Array[_ <: Any with Int]): Unit = {}
  def intARRAY_70(x: Array[_ <: Int with AnyVal]): Unit = {}
  def intARRAY_71(x: Array[_ <: AnyVal with Int]): Unit = {}
  def intARRAY_71a(x: Array[_ <: Int | Int]): Unit = {}
  def intARRAY_71b(x: Array[_ <: 1 | 2]): Unit = {}

  def stringARRAY_72(x: Array[String with Singleton]): Unit = {}
  def stringARRAY_73(x: Array[_ <: String]): Unit = {}
  def stringARRAY_74(x: Array[_ <: String with Singleton]): Unit = {}
  def stringARRAY_75(x: Array[_ <: Singleton with String]): Unit = {}
  def stringARRAY_76(x: Array[_ <: String with Any]): Unit = {}
  def stringARRAY_77(x: Array[_ <: Any with String]): Unit = {}
  def stringARRAY_78(x: Array[_ <: String with AnyRef]): Unit = {}
  def stringARRAY_79(x: Array[_ <: AnyRef with String]): Unit = {}
  def stringARRAY_79a(x: Array[_ <: String | String]): Unit = {}
  def stringARRAY_79b(x: Array[_ <: "a" | "b"]): Unit = {}

  def object_80(x: Array[_ <: Singleton]): Unit = {}
  def object_81(x: Array[_ <: AnyVal]): Unit = {}
  def objectARRAY_82(x: Array[_ <: AnyRef]): Unit = {}
  def object_83(x: Array[_ <: Any]): Unit = {}
  def object_83a(x: Array[_ <: Matchable]): Unit = {}
  def object_83b(x: Array[_ <: Int | Double]): Unit = {}
  def object_83c(x: Array[_ <: String | Int]): Unit = {}
  def object_83d(x: Array[_ <: Int | Matchable]): Unit = {}
  def object_83e(x: Array[_ <: AnyRef | AnyVal]): Unit = {}

  def serializableARRAY_84(x: Array[_ <: Serializable]): Unit = {}
  def univARRAY_85(x: Array[_ <: Univ]): Unit = {}
  def aARRAY_86(x: Array[_ <: A]): Unit = {}
  def aARRAY_87(x: Array[_ <: A with B]): Unit = {}

  def objectARRAY_88(x: Array[Any]): Unit = {}
  def objectARRAY_89(x: Array[AnyRef]): Unit = {}
  def objectARRAY_90(x: Array[AnyVal]): Unit = {}

  def nothing$ARRAY_91(x: Array[Nothing]): Unit = {}
  def null$ARRAY_92(x: Array[Null]): Unit = {}
  def nothing$ARRAY_93(x: Array[_ <: Nothing]): Unit = {}
  def null$ARRAY_94(x: Array[_ <: Null]): Unit = {}

}
