package b

import a.InnerClass
import a.InnerClassGen
import a.RawTypes
import a.InnerClassSub

object B {
  @main def test = {
    locally {
      val ici: InnerClass = new InnerClass()
      // val ici_inner1: ici.Inner[Long] = ici.createInner[Long](47L) // error
      val ici_inner2: InnerClass#Inner[Long] = ici.createInner[Long](47L)
      val ici_inner3: InnerClass#Inner[Long] = InnerClass.createInnerStatic[Long](47L)

      val ici_outer: InnerClass#Outer[Long] = new ici.Outer[Long]()
      val ici_nested1: InnerClass#Outer[Long]#Nested[Int] = new ici_outer.Nested[Int](47L, 23)
      val ici_nested2: InnerClass#Outer[Long]#Nested[Int] = ici.createNested[Long, Int](47L, 23)
      val ici_nested3: InnerClass#Outer[Long]#Nested[Int] = InnerClass.createNestedStatic[Long, Int](47L, 23)

      InnerClass.consumeNestedStatic(ici_nested3)
      InnerClass.consumeNestedStatic2(ici_nested3)
    }

    locally {
      val ici: InnerClassGen[String] = new InnerClassGen()
      // val ici_inner1: ici.Inner[Long] = ici.createInner[Long]("Hello", 47L) // error
      val ici_inner2: InnerClassGen[String]#Inner[Long] = ici.createInner[Long]("Hello", 47L)
      val ici_inner3: InnerClassGen[String]#Inner[Long] = InnerClassGen.createInnerStatic[String, Long]("Hello", 47L)

      val ici_outer: InnerClassGen[String]#Outer[Long] = new ici.Outer[Long]()
      val ici_nested1: InnerClassGen[String]#Outer[Long]#Nested[Int] = new ici_outer.Nested[Int]("Hello", 47L, 23)
      val ici_nested2: InnerClassGen[String]#Outer[Long]#Nested[Int] = ici.createNested[Long, Int]("Hello", 47L, 23)
      val ici_nested3: InnerClassGen[String]#Outer[Long]#Nested[Int] = InnerClassGen.createNestedStatic[String, Long, Int]("Hello", 47L, 23)

      InnerClassGen.consumeNestedStatic(ici_nested3)
    }

    locally {
      val rt: RawTypes = new RawTypes()
      val c: RawTypes#C[String] = new rt.C[String]()

      val cd_ii: RawTypes#C[String]#D[String] = new c.D[String]()
      val cd_ii_Raw: RawTypes#C[?]#D[?] = cd_ii

      RawTypes.mii_Raw_Raw(cd_ii_Raw)
      RawTypes.mii_Raw_Raw2(cd_ii_Raw)
    }

    locally {
      val ici: InnerClassSub = new InnerClassSub()
      // val ici_inner1: ici.Inner[Long] = ici.createInner[Long](47L) // error
      val ici_inner2: InnerClass#Inner[Long] = ici.createInnerSub[Long](47L)
      val ici_inner2_2: InnerClass#Inner[Long] = ici.createInnerSub2[Long](47L)
      val ici_inner3: InnerClass#Inner[Long] = InnerClassSub.createInnerStatic[Long](47L)

      val ici_outer: InnerClassSub#Outer[Long] = new ici.Outer[Long]()
      val ici_nested1: InnerClassSub#Outer[Long]#Nested[Int] = new ici_outer.Nested[Int](47L, 23)
      val ici_nested2: InnerClass#Outer[Long]#Nested[Int] = ici.createNestedSub[Long, Int](47L, 23)
      val ici_nested2_2: InnerClass#Outer[Long]#Nested[Int] = ici.createNestedSub2[Long, Int](47L, 23)
      val ici_nested3: InnerClass#Outer[Long]#Nested[Int] = InnerClassSub.createNestedStatic[Long, Int](47L, 23)

      InnerClass.consumeNestedStatic(ici_nested3)
      InnerClass.consumeNestedStatic2(ici_nested3)
    }
  }

}
