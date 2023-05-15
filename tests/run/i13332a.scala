import scala.deriving.Mirror

class Scope extends IListDefn {

  type OfK1[F[_], F0] = Mirror {
    type MirroredType[X] = F[X]
    type MirroredMonoType = F0
    type MirroredElemTypes[_] <: Tuple
  }

  type Of = OfK1[IList, IList[?]]

  // Force the children to be visited.
  type Foo = (Wrap.INil[Any], Wrap.ICons[Any], Wrap.Inner.Bottom[Any], Wrap.Inner.IZ[Any])

  final lazy val mirror_Scope_this_IList =
    summon[Of]
  final lazy val mirror_Scope_this_Wrap_ICons =
    type TICons = Tuple.Elem[mirror_Scope_this_IList.MirroredElemTypes[Any], 1]
    summon[Mirror.Of[TICons]]
  final lazy val mirror_Scope_this_Wrap_Inner_Bottom =
    type TBottom = Tuple.Elem[mirror_Scope_this_IList.MirroredElemTypes[Any], 2]
    summon[Mirror.Of[TBottom]]
}

trait IListDefn {
  sealed trait IList[A] // crucially, no companion is defined
  object Wrap {
    case class INil[A]() extends IList[A]
    case class ICons[A](h: A, t: IList[A]) extends IList[A]
    object Inner {
      sealed trait Bottom[A] extends IList[A]
      case class IZ[A]() extends Bottom[A]
    }
  }
}

sealed trait Opt[+A] // crucially, no companion is defined
case class Sm[+A](a: A) extends Opt[A]
case object Nn extends Opt[Nothing]

trait Module {

  sealed trait Lst // crucially, no companion is defined
  case class Cn(h: Int, t: Lst) extends Lst
  case object Nl extends Lst

  object knot extends Module

  final lazy val mirror_Module_this_knot_Lst =
    // here `Cn` and `Nl` are simultaneously reachable to `test`,
    // and also `knot` inherits from `Module`, there is no conflict here.
    summon[Mirror.Of[knot.Lst]]
}

class SubModule extends Module {
  final lazy val mirror_SubModule_this_Lst =
    summon[Mirror.Of[Lst]]
  final lazy val mirror_SubModule_this_knot_Lst =
    summon[Mirror.Of[knot.Lst]]
}

trait ForkedExample {
  object ParentHolder {
    sealed trait Parent // crucially, no companion is defined
  }
  object ChildHolder {
    case class Child(x: Int) extends ParentHolder.Parent
  }
}

class SubForkedExample extends ForkedExample {
  final lazy val mirror_SubForkedExample_this_ParentHolder_Parent =
    summon[Mirror.Of[ParentHolder.Parent]]
  final lazy val mirror_SubForkedExample_this_ChildHolder_Child =
    type TChild = Tuple.Head[mirror_SubForkedExample_this_ParentHolder_Parent.MirroredElemTypes]
    summon[Mirror.Of[TChild]]
}

trait BiggerKnot {

  sealed trait Lst // crucially, no companion is defined
  case class Cn(h: Int, t: Lst) extends Lst
  case object Nl extends Lst

  object Branch {
    object knot extends Module
  }


  final lazy val mirror_BiggerKnot_this_Branch_knot_Lst =
    // here `Cn` and `Nl` are simultaneously reachable to `test`,
    // and also `knot` inherits from `Module`, there is no conflict here.
    summon[Mirror.Of[Branch.knot.Lst]]
}

class SubBiggerKnot extends BiggerKnot {
  final lazy val mirror_SubBiggerKnot_this_Branch_knot_Lst =
    summon[Mirror.Of[Branch.knot.Lst]]
  final lazy val mirror_SubBiggerKnot_this_Branch_knot_Cn =
    type TCn = Tuple.Head[mirror_SubBiggerKnot_this_Branch_knot_Lst.MirroredElemTypes]
    summon[Mirror.Of[TCn]]
}

trait Universe {

  trait Container {
    object First {
      object Nested {
        sealed trait Lst // crucially, no companion is defined
      }
    }
    object Sibling {
      object Inner {
        case class Cn(h: Int, t: First.Nested.Lst) extends First.Nested.Lst
        case object Nl extends First.Nested.Lst
      }
    }
  }

  class BranchImpl {
    class SubContainer extends Container {
      final lazy val mirror_SubContainer_this_First_Nested_Lst =
        summon[Mirror.Of[First.Nested.Lst]]

      final lazy val mirror_SubContainer_this_Sibling_Inner_Cn =
        type TCn = Tuple.Head[mirror_SubContainer_this_First_Nested_Lst.MirroredElemTypes]
        summon[Mirror.Of[TCn]]
    }
    final lazy val impl = SubContainer()
  }

  final lazy val Branch = BranchImpl()

  final lazy val mirror_Universe_this_Branch_impl_First_Nested_Lst =
    summon[Mirror.Of[Branch.impl.First.Nested.Lst]]

  final lazy val mirror_Universe_this_Branch_impl_Sibling_Inner_Cn =
    type TCn = Tuple.Head[mirror_Universe_this_Branch_impl_First_Nested_Lst.MirroredElemTypes]
    summon[Mirror.Of[TCn]]
}

class SubUniverse extends Universe {
  final lazy val mirror_SubUniverse_this_Branch_impl_First_Nested_Lst =
    summon[Mirror.Of[Branch.impl.First.Nested.Lst]]
  final lazy val mirror_SubUniverse_this_Branch_impl_Sibling_Inner_Cn =
    type TCn = Tuple.Head[mirror_SubUniverse_this_Branch_impl_First_Nested_Lst.MirroredElemTypes]
    summon[Mirror.Of[TCn]]
}

trait Whole {
  trait MixinA {
    final lazy val mixinB = new MixinB() {}
  }
  trait MixinB {
    object A extends MixinB { // by inheriting `MixinB`, we should not check for inheritance from the right
      sealed trait Lst // crucially, no companion is defined
    }
    object Wrap {
      case class Cn(h: Int, t: A.Lst) extends A.Lst
      case object Nl extends A.Lst
    }
    object mixinA extends MixinA
  }
  trait SubAB extends MixinA with MixinB {
    val mirror_SubAB_this_mixinB_mixinA_mixinB_A_Lst =
      summon[Mirror.Of[SubAB.this.mixinB.mixinA.mixinB.A.Lst]]

    val mirror_SubAB_this_mixinB_mixinA_mixinB_Wrap_Cn =
      type TCn = Tuple.Head[mirror_SubAB_this_mixinB_mixinA_mixinB_A_Lst.MirroredElemTypes]
      summon[Mirror.Of[TCn]]
  }
}


@main def Test =
  locally {
    val s = Scope()
    val mirror_s_IList =
      summon[Mirror.Of[s.IList[Any]]]
    val mirror_s_Wrap_ICons =
      type TICons = Tuple.Elem[mirror_s_IList.MirroredElemTypes, 1]
      summon[Mirror.Of[TICons]]
    val mirror_s_Wrap_Inner_Bottom =
      type TBottom = Tuple.Elem[mirror_s_IList.MirroredElemTypes, 2]
      summon[Mirror.Of[TBottom]]
  }
  locally {
    val mirror_Opt =
      summon[Mirror.Of[`<empty>`.Opt[Any]]] // baseline
  }
  locally {
    val m = new Module() {}
    val mirror_m_Lst =
      summon[Mirror.Of[m.Lst]]
    val mirror_m_knot_Lst =
      summon[Mirror.Of[m.knot.Lst]]
  }
  locally {
    val sm = SubModule()
    val mirror_sm_Lst =
      summon[Mirror.Of[sm.Lst]]
    val mirror_sm_knot_Lst =
      summon[Mirror.Of[sm.knot.Lst]]
  }
  locally {
    val sf = SubForkedExample()
    val mirror_sf_ParentHolder_Parent =
      summon[Mirror.Of[sf.ParentHolder.Parent]]
    val mirror_sf_ChildHolder_Child =
      type TChild = Tuple.Head[mirror_sf_ParentHolder_Parent.MirroredElemTypes]
      summon[Mirror.Of[TChild]]
  }
  locally {
    val sbk = SubBiggerKnot()
    val mirror_sbk_Branch_knot_Lst =
      summon[Mirror.Of[sbk.Branch.knot.Lst]]
    val mirror_sbk_Branch_knot_Cn =
      type TCn = Tuple.Head[mirror_sbk_Branch_knot_Lst.MirroredElemTypes]
      summon[Mirror.Of[TCn]]
  }
  locally {
    val su = SubUniverse()
    val mirror_su_Branch_impl_First_Nested_Lst =
      summon[Mirror.Of[su.Branch.impl.First.Nested.Lst]]
    val mirror_su_Branch_impl_Sibling_Inner_Cn =
      type TCn = Tuple.Head[mirror_su_Branch_impl_First_Nested_Lst.MirroredElemTypes]
      summon[Mirror.Of[TCn]]
  }
