object collectionsEmpty {

  locally {
    val xs1 = List(1, 2, 3)
    val xs2 = xs1.empty
    val xs3: List[Int] = xs2
  }

  locally {
    val xs1 = Array(1, 2, 3)
    val xs2 = xs1.empty
    val xs3: Array[Int] = xs2
  }

  locally {
    val xs1 = Set(1, 2, 3)
    val xs2 = xs1.empty
    val xs3: Set[Int] = xs2
  }

  locally {
    val xs1 = "foo"
    val xs2 = xs1.empty
    val xs3: String = xs2
  }

  // Commented because there is no IsTraversableLike[Range] instance
  // locally {
  //   val xs1 = 1 to 3
  //   val xs2 = xs1.empty
  //   val xs3: IndexedSeq[Int] = xs2
  // }

  locally {
    val xs1 = Iterable(1, 2, 3)
    val xs2 = xs1.empty
    val xs3: Iterable[Int] = xs2
  }

}