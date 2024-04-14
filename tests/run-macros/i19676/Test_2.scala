//> using options -experimental -Yno-experimental

@companionToString("transformed by class")
class InPackage

@companionToString("transformed by object")
object InPackage

val (cls: Any, obj: Any) = {

  @companionToString("transformed by class")
  class InBlock

  @companionToString("transformed by object")
  object InBlock

  (new InBlock, InBlock)
}

object Wrapper {

  @companionToString("transformed by class")
  class InInnerClass

  @companionToString("transformed by object")
  object InInnerClass

}

@main def Test =
  assert((new InPackage).toString() == "InPackage: transformed by object")
  assert(InPackage.toString() == "InPackage$: transformed by class")
  assert(cls.toString() == "InBlock: transformed by object")
  assert(obj.toString() == "InBlock$: transformed by class")
  assert((new Wrapper.InInnerClass).toString() == "InInnerClass: transformed by object")
  assert(Wrapper.InInnerClass.toString() == "InInnerClass$: transformed by class")
