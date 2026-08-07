// https://github.com/scala/scala3/issues/26681
//
// Selecting on an unstable prefix skolemizes that prefix, and each selection must
// get its *own* skolem: two evaluations of `unstable` return two different `Owner`s,
// so their `w` are unrelated and `Box[w.type]` must not unify across occurrences.
//
// Memoizing the `QualSkolemType` per prefix type would make `maybeSkolemizePrefix`
// idempotent -- which is tempting, since a fresh skolem per call is what made the
// same selection carry two different skolems in i26681 -- but it also makes every
// case below compile. This test pins the property so that stays visible.
class Box[T](val x: T)

class Owner:
  val w: Int = 8
  val b: Box[w.type] = new Box(w)
  def get: Box[w.type] = new Box(w)
  def set(bb: Box[w.type]): Unit = ()

def unstable: Owner = new Owner

def same[T](a: Box[T], b: Box[T]): Unit = ()

def t1 = same(unstable.b, unstable.b) // error
def t2 = same(unstable.get, unstable.get) // error
def t3 = unstable.set(unstable.get) // error
