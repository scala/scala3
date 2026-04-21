//> using options -language:experimental.specializedTraits

inline trait Map[K: Specialized, V: Specialized]
inline trait MapFromInt[V: Specialized] extends Map[Int, V]
inline trait MapToInt[K: Specialized] extends Map[K, Int]
inline trait MapIntInt1 extends MapFromInt[Int]
inline trait MapIntInt2 extends MapToInt[Int]
