package scala
package collection
package immutable

import HashMap.{ HashTrieMap, HashMapCollision1, HashMap1 }
import HashSet.{ HashTrieSet, HashSetCollision1, HashSet1 }

object i996{

  private[this] def collisionToArray[T](x: Iterable[T]): Array[Iterable[T]] = (x match {
    case x: HashMapCollision1[_, _] => x.kvs.map(x => HashMap(x)).toArray
    case x: HashSetCollision1[_]    => x.ks.map(x => HashSet(x)).toArray
  }).asInstanceOf[Array[Iterable[T]]]

}
