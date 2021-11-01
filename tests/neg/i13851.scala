opaque type One = 1
inline val One: One = 1 // error

opaque type Max = Int.MaxValue.type
inline val Max: Max = Int.MaxValue // error

inline val MaxValue: Int.MaxValue.type = Int.MaxValue

opaque type Two = 2
type Bis = Two
inline val Two: Bis = 2 // error