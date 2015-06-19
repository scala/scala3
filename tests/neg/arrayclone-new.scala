// Run with -explaintypes to see information about shadowing failures
import scala.reflect.{ClassTag, classTag}

object Test extends dotty.runtime.LegacyApp{
  ObjectArrayClone;
  PolymorphicArrayClone;
}

object ObjectArrayClone{
  val it : Array[String] = Array("1", "0");
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = "0";
  assert(it(0) == "1")
}

object PolymorphicArrayClone{
  def testIt[T](it : Array[T], one : T, zero : T) = {
    val cloned = it.clone();
    assert(cloned.sameElements(it));
    cloned(0) = zero;
    assert(it(0) == one)
  }

  testIt(Array("one", "two"), "one", "two");

  class Mangler[T: ClassTag](ts : T*){
    // this will always be a BoxedAnyArray even after we've unboxed its contents.
    val it = ts.toArray[T];
  }

  val mangled = new Mangler[Int](0, 1);

  val y : Array[Int] = mangled.it; // make sure it's unboxed

  testIt(mangled.it, 0, 1);
}

