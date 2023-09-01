import scala.deriving.Mirror

package lib {

  case class NewFoo(x: Int = 1, y: Int)

  object NewMirrors {
    val mNewFoo = summon[Mirror.Of[NewFoo]]

    val mOldFoo = summon[Mirror.Of[OldFoo]]
    val mOldBar = summon[Mirror.Of[OldBar]]
  }
}

package app {
  import lib.*

  object Main {

    // defaultArgument implementation did not throw NoSuchElementException
    def foundDefaultArgument(m: Mirror.Product): Boolean = try {
      m.defaultArgument(0)
      true
    } catch {
      case _: NoSuchElementException => false
    }

    def main(args: Array[String]): Unit = {

      // NewFoo: normal case with support for default arguments

      assert(NewMirrors.mNewFoo.defaultArgument(0) == 1)
      summon[NewMirrors.mNewFoo.MirroredElemHasDefaults =:= (true, false)]

      // OldFoo: does not override the defaultArgument implementation

      assert(!foundDefaultArgument(NewMirrors.mOldFoo)) // Expected: since mirror of old case class
      summon[NewMirrors.mOldFoo.MirroredElemHasDefaults =:= (false, false)] // Necessary: to be consistent with defaultArgument implementation

      assert(!foundDefaultArgument(OldMirrors.mOldFoo)) // Expected: since mirror of old case class
      summon[scala.util.NotGiven[OldMirrors.mOldFoo.MirroredElemHasDefaults <:< (Boolean, Boolean)]] // reference to old mirror doesn't have any refinement
      summon[OldMirrors.mOldFoo.MirroredElemHasDefaults <:< Tuple] // but does inherit type member from Mirror trait

      // OldBar: is anon mirror so could implement defaultArgument
      // but we manually keep behaviour consistent with other mirrors of old case classes

      assert(NewMirrors.mOldBar ne lib.OldBar)
      assert(!foundDefaultArgument(NewMirrors.mOldBar))
      summon[NewMirrors.mOldBar.MirroredElemHasDefaults =:= (false, false)] // Ok: should be consistent with above

      assert(OldMirrors.mOldBar ne lib.OldBar)
      assert(!foundDefaultArgument(OldMirrors.mOldBar))
      summon[scala.util.NotGiven[OldMirrors.mOldBar.MirroredElemHasDefaults <:< (Boolean, Boolean)]]
      summon[OldMirrors.mOldBar.MirroredElemHasDefaults <:< Tuple]

    }
  }
}
