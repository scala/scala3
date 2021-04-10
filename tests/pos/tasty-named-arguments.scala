trait Fn[-T, R] { def apply(t: T): R }

val f: Fn[Int, Int] { def apply(arg: Int): Int } = x => x
val g = f.apply(arg = 1)

val m: [T] => (arg: T) => T = [T] => (arg: T) => arg
val n = m.apply(arg = 23)

trait KeyValuePair { type Value; def value: Value }

val d: (kvp: KeyValuePair) => kvp.Value = _.value
val e = d.apply(kvp = new { type Value = Int; val value = 23 } )

val c: (i: Int) ?=> i.type = x ?=> x
val i = c.apply(using 0)
