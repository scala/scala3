import deriving.Mirror

val mFoo = summon[Mirror.Of[Foo]] // error: `Foo.<init>(Int)` is not accessible from `<empty>`.
