
trait Trait :
  type Y
  var list: List[Y] = Nil

class Class[Y] extends Trait :
  def add(elm: Y): Unit = list = elm :: list // error

object Object extends Class[Int] :
  add(42)
