//> using options -Yno-flexible-types

import marked.*
import marked.sub.K
import unmarked.*

// Null-marked package
def j1(j: J): String = j.field
def j2(j: J): String = j.nullableField // error
def j3: J = J(null) // error
def j4: J = J(null, null) // `@NullUnmarked` constructor
def j5(j: J): String = j.get()
def j6(j: J): String = j.getNullable() // error
def j7(j: J): Unit = j.set(null) // error
def j8(j: J): Unit = j.setNullable(null)
def j9(j: J): String = j.unmarkedGet() // error
def j10(j: J): String = j.bothGet()
def j11: String = J.staticGet()
def j12: String = J.staticGetNullable() // error
def j13(n: J.Nested): String = n.get()
def j14: String = J.Nested.staticGet()
def j15(i: J#Inner): String = i.get()
def j16(n: J.UnmarkedNested): String = n.get() // error
def j17: String = J.UnmarkedNested.staticGet() // error
def j18(n: J.UnmarkedNested): String = n.markedGet()

// `@NullUnmarked` class in a null-marked package
def u1(u: U): String = u.get() // error
def u2: String = U.staticGet() // error
def u3(u: U): String = u.markedGet()
def u4(u: U): String = u.bothGet() // error

// Subpackage of a null-marked package
def k1(k: K): String = k.get() // error

// Kotlin classes
def kt1(k: Kt): String = k.get() // error
def kt2(k: Kt.Nested): String = k.get() // error
def kt3(k: KtMarked): String = k.get()

// Null-marked class in a package that is not null-marked
def c1(c: C): String = c.get()
def c2: String = C.staticGet()
def c3: Unit = C.staticSet(null) // error

// Null-marked method and constructor in a class that is not null-marked
def d1(d: D): String = d.get() // error
def d2(d: D): String = d.markedGet()
def d3: D = D(null) // error

// Generics
def g1(g: G[String, String]): String = g.get()
def g2(g: G[String | Null, String]): String = g.get() // error
def g3(g: G[String | Null, String]): String | Null = g.get()
def g4(g: G[String, String]): String = g.getNullable() // error
def g5(g: G[String | Null, String]): Unit = g.set(null, "")
def g6(g: G[String, String]): Unit = g.set("", null) // error
def g7(g: G[String, String]): String = g.id("")
def g8(g: G[String, String]): java.util.List[String] = g.list()
def g9(g: G[String, String]): java.util.List[String] = g.nullableList() // error
def g10(g: G[String, String]): java.util.List[String | Null] = g.nullableList()
def g11(g: G[String, String]): java.util.List[? <: CharSequence] = g.wildcard() // error
def g12(g: G[String, String]): java.util.List[? <: CharSequence | Null] = g.wildcard()
def g13(g: G[String, String]): java.util.List[?] = g.unboundedWildcard()
def g14(g: G[String, String]): java.util.Map[String, java.util.List[String]] = g.nested() // error
def g15(g: G[String, String]): java.util.Map[String, java.util.List[String] | Null] = g.nested()
