class Bag(seq: Seq[Char])

inline val i = 2
inline val j: Int = 2 // error
inline val b: Boolean = true // error
inline val s: String = "" // error
inline val bagA = new Bag(Seq('a', 'b', 'c')) // error
inline val bagB: Bag = new Bag(Seq('a', 'b', 'c')) // error
