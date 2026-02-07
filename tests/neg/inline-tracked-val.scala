import language.experimental.modularity

inline val v1 = 3
inline val v2: Short = 3
inline val v3: Int = 3
inline val v4 = 3: Short
inline val v5 = 3: Int
inline val v6: Short = 3: Short
inline val v7: Short = 3: Int // error
inline val v8: Int = 3: Short
inline val v9: Int = 3: Int

// The same tests with `tracked` should behave the same way

tracked inline val v1t = 3
tracked inline val v2t: Short = 3
tracked inline val v3t: Int = 3
tracked inline val v4t = 3: Short
tracked inline val v5t = 3: Int
tracked inline val v6t: Short = 3: Short
tracked inline val v7t: Short = 3: Int // error
tracked inline val v8t: Int = 3: Short
tracked inline val v9t: Int = 3: Int

@main def Test() =
  summon[v1.type =:= 3]
  summon[v2.type <:< Short]
  summon[v3.type =:= 3]
  summon[v4.type <:< Short]
  summon[v5.type <:< Int]
  summon[v5.type <:< 3] // error
  summon[v6.type <:< Short]
  summon[v7.type <:< Short]
  summon[v8.type <:< Int]
  summon[v8.type <:< 3] // error
  summon[v9.type <:< Int]
  summon[v9.type <:< 3] // error

  summon[v1t.type =:= 3]
  summon[v2t.type <:< Short]
  summon[v3t.type =:= 3]
  summon[v4t.type <:< Short]
  summon[v5t.type <:< Int]
  summon[v5t.type <:< 3] // error
  summon[v6t.type <:< Short]
  summon[v7t.type <:< Short]
  summon[v8t.type <:< Int]
  summon[v8t.type <:< 3] // error
  summon[v9t.type <:< Int]
  summon[v9t.type <:< 3] // error
