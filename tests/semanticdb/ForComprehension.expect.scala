package example

class ForComprehension/*<<=example.ForComprehension#*/ {
  for {
    a <- List/*=>>scala.package.List.*//*=>>scala.collection.IterableFactory#apply().*/(1)/*=>>scala.collection.immutable.List#flatMap().*/
    b/*=>>local0*/ <- List/*=>>scala.package.List.*//*=>>scala.collection.IterableFactory#apply().*/(1)/*=>>scala.collection.IterableOps#withFilter().*/
    if b/*=>>local0*/ >/*=>>scala.Int#`>`(+3).*/ 1/*=>>scala.collection.WithFilter#map().*/
    c/*=>>local2*/ = a/*=>>local1*/ +/*=>>scala.Int#`+`(+4).*/ b/*=>>local0*//*=>>scala.collection.immutable.List#map().*/
  } yield (a/*=>>local1*/, b/*=>>local0*/, c/*=>>local2*/)
  for {
    a <- List/*=>>scala.package.List.*//*=>>scala.collection.IterableFactory#apply().*/(1)/*=>>scala.collection.immutable.List#flatMap().*/
    b <- List/*=>>scala.package.List.*//*=>>scala.collection.IterableFactory#apply().*/(a/*=>>local3*/)/*=>>scala.collection.IterableOps#withFilter().*/
    if (
      a/*=>>local3*/,
      b/*=>>local4*/
    ) ==/*=>>scala.Any#`==`().*/ (1, 2)/*=>>scala.collection.WithFilter#flatMap().*/
    (
      c,
      d
    ) <- List/*=>>scala.package.List.*//*=>>scala.collection.IterableFactory#apply().*/((a/*=>>local3*/, b/*=>>local4*/))/*=>>scala.collection.WithFilter#withFilter().*//*=>>scala.collection.IterableOps#withFilter().*/
    if (
      a/*=>>local3*/,
      b/*=>>local4*/,
      c/*=>>local5*/,
      d/*=>>local6*/
    ) ==/*=>>scala.Any#`==`().*/ (1, 2, 3, 4)/*=>>scala.collection.WithFilter#map().*/
    e/*=>>local7*/ = (
      a/*=>>local3*/,
      b/*=>>local4*/,
      c/*=>>local5*/,
      d/*=>>local6*/
    )/*=>>scala.collection.IterableOps#withFilter().*/
    if e/*=>>local7*/ ==/*=>>scala.Any#`==`().*/ (1, 2, 3, 4)/*=>>scala.collection.WithFilter#flatMap().*/
    f <- List/*=>>scala.package.List.*//*=>>scala.collection.IterableFactory#apply().*/(e/*=>>local7*/)/*=>>scala.collection.immutable.List#map().*/
  } yield {
    (
      a/*=>>local3*/,
      b/*=>>local4*/,
      c/*=>>local5*/,
      d/*=>>local6*/,
      e/*=>>local7*/,
      f/*=>>local8*/
    )
  }
}
