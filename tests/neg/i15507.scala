object TestNested:
  type _NestedSet1[X] = Set[_NestedSet1[?]] // error // error
  type _NestedSet2[X] <: Set[_NestedSet2[?]] // error
  type _NestedSet3[X] <: Set[_NestedSet3[X]] // ok
  type _NestedSet4[X] >: Set[_NestedSet4[X]] // error
  type _NestedSet5[X] = Set[_NestedSet5[X]] // error
  type _NestedSet6[X] = Set[_NestedSet6[Int]] // error

  type _NestedList1[X] = List[_NestedList1[?]] // error // error
  type _NestedList2[X] <: List[_NestedList2[?]] // error
  type _NestedList3[X] <: List[_NestedList3[X]] // ok
  type _NestedList4[X] >: List[_NestedList4[X]] // error
  type _NestedList5[X] = List[_NestedList5[X]] // error
  type _NestedList6[X] = List[_NestedList6[Int]] // error

