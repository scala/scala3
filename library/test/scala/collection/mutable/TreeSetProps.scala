package scala.collection.mutable

import org.scalacheck.Prop.{forAll, throws}
import org.scalacheck.Properties
import org.scalacheck.Gen

object TreeSetProps extends Properties("Mutable TreeSet") {

  val generator = Gen.listOfN(1000, Gen.chooseNum(0, 1000))

  val denseGenerator = Gen.listOfN(1000, Gen.chooseNum(0, 200))

  property("Insertion doesn't allow duplicates values.") = forAll(generator) { (s: List[Int]) =>
    {
      val t = TreeSet[Int](s*)
      t == s.toSet
    }
  }

  property("Verification of size method validity") = forAll(generator) { (s: List[Int]) =>
    {
      val t = TreeSet[Int](s*)
      for (a <- s) {
        t -= a
      }
      t.size == 0
    }
  }

  property("All inserted elements are removed") = forAll(generator) { (s: List[Int]) =>
    {
      val t = TreeSet[Int](s*)
      for (a <- s) {
        t -= a
      }
      t == Set()
    }
  }

  property("Elements are sorted.") = forAll(generator) { (s: List[Int]) =>
    {
      val t = TreeSet[Int](s*)
      t.toList == s.distinct.sorted
    }
  }

  property("Implicit CanBuildFrom resolution succeeds as well as the \"same-result-type\" principle.") =
    forAll(generator) { (s: List[Int]) =>
      {
        val t = TreeSet[Int](s*)
        val t2 = t.map(_ * 2)
        t2.isInstanceOf[TreeSet[Int]]
      }
    }

  property("A view doesn't expose off bounds elements") = forAll(denseGenerator) { (s: List[Int]) =>
    {
      val t = TreeSet[Int](s*)
      val view = t.rangeImpl(Some(50), Some(150))
      view.filter(_ < 50) == Set[Int]() && view.filter(_ >= 150) == Set[Int]()
    }
  }

  property("ordering must not be null") =
    throws(classOf[NullPointerException])(TreeSet.empty[Int](using null.asInstanceOf[Ordering[Int]]))
}
