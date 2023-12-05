import IArray.{+:, ++:}
def test(arr: IArray[Int]): Unit =
  1 +: arr;
  arr.+:(1);
  +:(arr)(1);

  arr ++: arr;
  arr.++:(arr);
  ++:(arr)(arr);

  Nil ++: arr;
  arr.++:(Nil);
  ++:(arr)(Nil);
