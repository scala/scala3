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
  def a_01(a: A & B): Unit = {}
  def a_02X(b: B & A): Unit = {}
  def a_02(a: A & B & A): Unit = {}
  def a_03(a: A & (B & A)): Unit = {}
  def a_04(b: A & (B & A) @foo): Unit = {}
  def a_04X(b: A & (B & C) @foo): Unit = {}
  def a_05(b: A & (B & A) @foo & (C & B & A) @foo): Unit = {}

  type T1 <: A & B
  def a_06(a: T1): Unit = {}

  type S <: B & T1
  def a_07(a: S): Unit = {}

  type T2 <: B & A
  type U <: T2 & S
  def a_08(b: U): Unit = {}

  val singB: B = new B {}
  def a_09(a: A & singB.type): Unit = {}
  def a_10(b: singB.type & A): Unit = {}

  type V >: SubB <: B
  def b_11(b: V): Unit = {}
  def subb_12(b: V & SubB): Unit = {}

  def d_13(d: D & A): Unit = {}
  def d_14(d: A & D): Unit = {}

  val singD: D = new D {}
  def d_13x(d: singD.type & A): Unit = {}
  def d_14x(d: A & singD.type): Unit = {}

  type DEq = D
  def d_15(d: A & DEq): Unit = {}
  def d_16(d: A & (DEq @foo)): Unit = {}
  def d_17(d: DEq & A): Unit = {}
  def d_18(d: (DEq @foo) & A): Unit = {}

  val singDEq: DEq @foo = new D {}
  def d_15b(d: A & singDEq.type): Unit = {}
  def d_16b(d: A & (singDEq.type @foo)): Unit = {}

  type DSub <: D
  def d_19(a: A & DSub): Unit = {}
  def d_19x(d: DSub & A): Unit = {}
  def d_20(z: DSub & Z): Unit = {}

  type W1 <: A & Cov[Any]
  type X1 <: Cov[Int] & W1
  def a_21(a: X1): Unit = {}

  type W2 <: A & Cov[Any]
  type X2 <: Cov[Int] & W2
  def a_22(a: X2): Unit = {}

  def z_23(z: A & this.type): Unit = {}
  def z_24(z: this.type & A): Unit = {}

  def a_25(b: A & (B { type T })): Unit = {}
  def a_26(a: (A { type T }) & ((B & A) { type T })): Unit = {}

  def a_27(a: VC & B): Unit = {}
  def a_28(a: B & VC): Unit = {}

  val o1: Outer = new Outer
  val o2: Outer = new Outer
  def e_29(f: o1.E & o1.F): Unit = {}
  def e_30(f: o1.F & o1.E): Unit = {}
  def e_31(f: o1.E & o2.F): Unit = {}
  def e_32(f: o2.F & o1.E): Unit = {}
  def e_33(f: Outer#E & Outer#F): Unit = {}
  def e_34(f: Outer#F & Outer#E): Unit = {}

  val structural1: { type DSub <: D } = new { type DSub <: D }
  def d_35(a: A & structural1.DSub): Unit = {}
  def d_36(a: structural1.DSub & A): Unit = {}
  def d_37(z: Z & structural1.DSub): Unit = {}
  def d_38(z: structural1.DSub & Z): Unit = {}

  val structural2: { type SubCB <: C & B } = new { type SubCB <: C & B }
  def b_39(c: structural2.SubCB & B): Unit = {}
  def b_40(c: B & structural2.SubCB): Unit = {}

  val structural3a: { type SubB <: B; type SubCB <: C & SubB } = new { type SubB <: B; type SubCB <: C & SubB }
  val structural3b: { type SubB <: B; type SubCB <: C & SubB } = new { type SubB <: B; type SubCB <: C & SubB }
  def b_41(c: structural3a.SubB & structural3a.SubCB): Unit = {}
  def b_42(c: structural3a.SubCB & structural3a.SubB): Unit = {}
  def b_43(b: structural3a.SubB & structural3b.SubCB): Unit = {}
  def b_44(c: structural3b.SubCB & structural3a.SubB): Unit = {}

  type SubStructural <: C & structural3a.SubB
  def b_45(x: structural3a.SubB & SubStructural): Unit = {}
  def b_46(x: structural3b.SubB & SubStructural): Unit = {}

  type Rec1 <: A & B
  type Rec2 <: C & Rec1
  def a_47(a: A & B & Rec2): Unit = {}
  def a_48(a: (A & B) @foo & Rec2): Unit = {}

  type F1 = A & B
  type F2 = A & B
  type Rec3 <: F1
  type Rec4 <: C & Rec3
  def a_49(a: F1 @foo & Rec4): Unit = {}
  def a_50(a: F1 & Rec4): Unit = {}
  def a_51(a: F2 @foo & Rec4): Unit = {}
  def a_52(a: F2 & Rec4): Unit = {}

  type AA = A
  type F3 = AA & B
  type Rec5 <: F3
  type Rec6 <: C & Rec5
  def a_53(a: F3 @foo & Rec6): Unit = {}
  def a_54(a: F3 & Rec6): Unit = {}

  val structural4a: { type M[X] <: A } = new { type M[X] <: A }
  val structural4b: { type N <: B & structural4a.M[Int] } = new { type N <: B & structural4a.M[Int] }
  def a_55(x: structural4a.M[Any] & structural4b.N): Unit = {}

  type Bla = A { type M[X] <: A }
  def a_56(x: Bla#M[Any] & ({ type N <: B & Bla#M[Int] })#N): Unit = {}
  type AEq = A
  type Bla2 = AEq { type M[X] <: A }
  def a_57(x: Bla2#M[Any] & ({ type N <: B & Bla2#M[Int] })#N): Unit = {}

  def int_58(x: Int & Singleton): Unit = {}
  def int_59(x: Singleton & Int): Unit = {}
  def int_60(x: Int & Any): Unit = {}
  def int_61(x: Any & Int): Unit = {}
  def int_62(x: Int & AnyVal): Unit = {}
  def int_63(x: AnyVal & Int): Unit = {}

  def intARRAY_64(x: Array[Int & Singleton]): Unit = {}
  def intARRAY_65(x: Array[_ <: Int]): Unit = {}
  def intARRAY_66(x: Array[_ <: Int & Singleton]): Unit = {}
  def intARRAY_67(x: Array[_ <: Singleton & Int]): Unit = {}
  def intARRAY_68(x: Array[_ <: Int & Any]): Unit = {}
  def intARRAY_69(x: Array[_ <: Any & Int]): Unit = {}
  def intARRAY_70(x: Array[_ <: Int & AnyVal]): Unit = {}
  def intARRAY_71(x: Array[_ <: AnyVal & Int]): Unit = {}
  def intARRAY_71a(x: Array[_ <: Int | Int]): Unit = {}
  def intARRAY_71b(x: Array[_ <: 1 | 2]): Unit = {}

  def stringARRAY_72(x: Array[String & Singleton]): Unit = {}
  def stringARRAY_73(x: Array[_ <: String]): Unit = {}
  def stringARRAY_74(x: Array[_ <: String & Singleton]): Unit = {}
  def stringARRAY_75(x: Array[_ <: Singleton & String]): Unit = {}
  def stringARRAY_76(x: Array[_ <: String & Any]): Unit = {}
  def stringARRAY_77(x: Array[_ <: Any & String]): Unit = {}
  def stringARRAY_78(x: Array[_ <: String & AnyRef]): Unit = {}
  def stringARRAY_79(x: Array[_ <: AnyRef & String]): Unit = {}
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
  def aARRAY_87(x: Array[_ <: A & B]): Unit = {}

  def objectARRAY_88(x: Array[Any]): Unit = {}
  def objectARRAY_89(x: Array[AnyRef]): Unit = {}
  def objectARRAY_90(x: Array[AnyVal]): Unit = {}

  def nothing$ARRAY_91(x: Array[Nothing]): Unit = {}
  def null$ARRAY_92(x: Array[Null]): Unit = {}
  def nothing$ARRAY_93(x: Array[_ <: Nothing]): Unit = {}
  def null$ARRAY_94(x: Array[_ <: Null]): Unit = {}

}
