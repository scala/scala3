# Todo List
Differences between scalameta implementation and dotc.

- [ ] For comprehension in Synthetic section.
- [ ] Implicit conversions in Synthetic section.
- [ ] Record all definitions in Symbols section.

## ValPatterns

Val patterns are tricky due to desugaring before ExtractSemanticDB sees them.

### Tuples
```scala
val (left, right) = (1, 2)
```
desugars to
```scala
private[this] <synthetic> val $3$: (Int, Int) =  Tuple2.apply[Int, Int](1, 2)
val left: Int = this.$3$._1
val right: Int = this.$3$._2
```

### Product1 Pattern
```scala
val Some(number1) = Some(1)
```
desugars to
```scala
val number1: Int = Some.apply[Int](1):Some[Int] @unchecked match {
  case Some.unapply[Int](number1 @ _):Some[Int] => number1:Int
}
```

### ProductN Pattern
```scala
val x #:: xs = LazyList(1, 2)
```
desugars to
```scala
private[this] <synthetic> val $2$: (Int, LazyList[Int]) =
  LazyList.apply[Int]([1,2 : Int]:Int*):LazyList[Int] @unchecked match {
    case #::.unapply[Int](x @ _, xs @ _):LazyList[Int] => Tuple2.apply[Int, LazyList[Int]](x, xs)
  }
val x: Int = this.$2$._1
val xs: LazyList[Int] = this.$2$._2
```

perhaps it is safe to recognise these patterns if the binds were made synthetic.
