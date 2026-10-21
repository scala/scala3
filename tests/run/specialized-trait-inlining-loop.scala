//> using options -language:experimental.specializedTraits
inline trait T1[T: Specialized]:
	def bar1 = myInlineMethod1[T]

inline trait T2[T: Specialized]:
	def bar2 = myInlineMethod2[T]

inline trait T3[T: Specialized]:
	def foo(x: T): T = x

inline def myInlineMethod1[T: Specialized] = new T2[T]() {}
inline def myInlineMethod2[T: Specialized] = new T3[T]() {}

@main def Test =
	val x = new T1[Int]() {}
	assert(x.bar1.bar2.foo(10) == 10)

// Need to:
// 1) Create T1$impl$Int$ and inline `def bar1 = myInlineMethod1[Int]` (desugarSpecializedTraits)
// 2) Inline myInlineMethod1 into bar1 inside T1$impl$Int (inlining)
// 3) Create T2$impl$Int and fix the inlined definition of bar1/myInlineMethod1 (desugarSpecializedTraits)
// 4) Inline myInlineMethod2 into bar2 inside T2$impl$Int
// 5) Create T3$impl$Int and fix the inlined definition of bar2/myInlineMethod2 (desugarSpecializedTraits) 

// This can continue for arbitrarily many inlines and you don't know in advance that any of these specializations need to be created.
