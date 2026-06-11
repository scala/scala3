inline trait GreatGrandParent:
  val x = 10
inline trait GrandParent extends GreatGrandParent:
  override val x = 11
inline trait Parent extends GrandParent:
  override val x = 12
class C extends Parent

@main def Test =
  val x = C()
  assert(x.x == 12)
