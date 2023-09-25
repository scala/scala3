val wrongLength1: [T, S] => (T, S) => T = [T] => (x, y) => x // error
val wrongLength2: [T] => T => T = [T] => (x, x) => x // error

val notSubType: [T] => T => T = [T <: Int] => x => x // error

val notInScope: [T] => T => T = [S] => x => (x: T) // error
