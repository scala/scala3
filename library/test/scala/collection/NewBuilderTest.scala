package scala.collection

import scala.{collection => sc}, sc.{mutable => scm, immutable => sci}
import scala.reflect.ClassTag

import org.junit.Assert.*
import org.junit.Test

class NewBuilderTest {

  @deprecated("Tests old collection types", since="2.13.0")
  @Test
  def mapPreservesCollectionType(): Unit = {
    def test[T: ClassTag](mapped: Any): Unit = {
      val expected = reflect.classTag[T].runtimeClass
      val isInstance = reflect.classTag[T].runtimeClass.isInstance(mapped)
      assertTrue(s"$mapped (of class ${mapped.getClass} is not a in instance of ${expected}", isInstance)
    }

    test[sc.GenTraversable[?]   ]((sc.GenTraversable(1):    sc.GenTraversable[Int]).map(x => x))
    test[sc.Traversable[?]      ]((sc.Traversable(1):       sc.GenTraversable[Int]).map(x => x))
    test[sc.GenIterable[?]      ]((sc.GenIterable(1):       sc.GenTraversable[Int]).map(x => x))
    test[sc.Iterable[?]         ]((sc.Iterable(1):          sc.GenTraversable[Int]).map(x => x))
    test[sc.GenSeq[?]           ]((sc.GenSeq(1):            sc.GenTraversable[Int]).map(x => x))
    test[sc.Seq[?]              ]((sc.Seq(1):               sc.GenTraversable[Int]).map(x => x))
    test[sc.LinearSeq[?]        ]((sc.LinearSeq(1):         sc.GenTraversable[Int]).map(x => x))
    test[sc.LinearSeq[?]        ]((sc.LinearSeq(1):         sc.Seq[Int]           ).map(x => x))
    test[sc.IndexedSeq[?]       ]((sc.IndexedSeq(1):        sc.GenTraversable[Int]).map(x => x))
    test[sc.IndexedSeq[?]       ]((sc.IndexedSeq(1):        sc.Seq[Int]           ).map(x => x))
    test[sc.GenSet[?]           ]((sc.GenSet(1):            sc.GenTraversable[Int]).map(x => x))
    test[sc.Set[?]              ]((sc.Set(1):               sc.GenTraversable[Int]).map(x => x))
    test[sc.GenMap[?, ?]        ]((sc.GenMap(1 -> 1):       sc.GenMap[Int, Int]   ).map(x => x))
    test[sc.Map[?, ?]           ]((sc.Map(1 -> 1):          sc.GenMap[Int, Int]   ).map(x => x))

    test[scm.Traversable[?]     ]((scm.Traversable(1):      sc.GenTraversable[Int]).map(x => x))
    test[scm.Iterable[?]        ]((scm.Iterable(1):         sc.GenTraversable[Int]).map(x => x))
    //test[scm.LinearSeq[?]       ]((scm.LinearSeq(1):        sc.GenTraversable[Int]).map(x => x))
    //test[scm.LinearSeq[?]       ]((scm.LinearSeq(1):        sc.Seq[Int]           ).map(x => x))
    //test[scm.MutableList[?]     ]((scm.MutableList(1):      sc.GenTraversable[Int]).map(x => x))
    //test[scm.MutableList[?]     ]((scm.MutableList(1):      sc.Seq[Int]           ).map(x => x))
    test[scm.Queue[?]           ]((scm.Queue(1):            sc.GenTraversable[Int]).map(x => x))
    test[scm.Queue[?]           ]((scm.Queue(1):            sc.Seq[Int]           ).map(x => x))
    //test[scm.DoubleLinkedList[?]]((scm.DoubleLinkedList(1): sc.GenTraversable[Int]).map(x => x))
    //test[scm.DoubleLinkedList[?]]((scm.DoubleLinkedList(1): sc.Seq[Int]           ).map(x => x))
    //test[scm.LinkedList[?]      ]((scm.LinkedList(1):       sc.GenTraversable[Int]).map(x => x))
    //test[scm.LinkedList[?]      ]((scm.LinkedList(1):       sc.Seq[Int]           ).map(x => x))
    //test[scm.ArrayStack[?]      ]((scm.ArrayStack(1):       sc.GenTraversable[Int]).map(x => x))
    //test[scm.ArrayStack[?]      ]((scm.ArrayStack(1):       sc.Seq[Int]           ).map(x => x))
    //test[scm.Stack[?]           ]((scm.Stack(1):            sc.GenTraversable[Int]).map(x => x))
    //test[scm.Stack[?]           ]((scm.Stack(1):            sc.Seq[Int]           ).map(x => x))
    test[scm.ArraySeq[?]        ]((scm.ArraySeq(1):         sc.GenTraversable[Int]).map(x => x))
    test[scm.ArraySeq[?]        ]((scm.ArraySeq(1):         sc.Seq[Int]           ).map(x => x))

    test[scm.Buffer[?]          ]((scm.Buffer(1):           sc.GenTraversable[Int]).map(x => x))
    test[scm.Buffer[?]          ]((scm.Buffer(1):           sc.Seq[Int]           ).map(x => x))
    test[scm.IndexedSeq[?]      ]((scm.IndexedSeq(1):       sc.GenTraversable[Int]).map(x => x))
    test[scm.IndexedSeq[?]      ]((scm.IndexedSeq(1):       sc.Seq[Int]           ).map(x => x))
    test[scm.ArrayBuffer[?]     ]((scm.ArrayBuffer(1):      sc.GenTraversable[Int]).map(x => x))
    test[scm.ArrayBuffer[?]     ]((scm.ArrayBuffer(1):      sc.Seq[Int]           ).map(x => x))
    test[scm.ListBuffer[?]      ]((scm.ListBuffer(1):       sc.GenTraversable[Int]).map(x => x))
    test[scm.ListBuffer[?]      ]((scm.ListBuffer(1):       sc.Seq[Int]           ).map(x => x))
    test[scm.Seq[?]             ]((scm.Seq(1):              sc.GenTraversable[Int]).map(x => x))
    test[scm.Seq[?]             ]((scm.Seq(1):              sc.Seq[Int]           ).map(x => x))
    //test[scm.ResizableArray[?]  ]((scm.ResizableArray(1):   sc.GenTraversable[Int]).map(x => x))
    //test[scm.ResizableArray[?]  ]((scm.ResizableArray(1):   sc.Seq[Int]           ).map(x => x))
    test[scm.Set[?]             ]((scm.Set(1):              sc.GenTraversable[Int]).map(x => x))
    test[scm.Set[?]             ]((scm.Set(1):              sc.Set[Int]           ).map(x => x))
    test[scm.HashSet[?]         ]((scm.HashSet(1):          sc.GenTraversable[Int]).map(x => x))
    test[scm.HashSet[?]         ]((scm.HashSet(1):          sc.Set[Int]           ).map(x => x))
    test[scm.LinkedHashSet[?]   ]((scm.LinkedHashSet(1):    sc.GenTraversable[Int]).map(x => x))
    test[scm.LinkedHashSet[?]   ]((scm.LinkedHashSet(1):    sc.Set[Int]           ).map(x => x))

    test[sci.Traversable[?]     ]((sci.Traversable(1):      sc.GenTraversable[Int]).map(x => x))
    test[sci.Iterable[?]        ]((sci.Iterable(1):         sc.GenTraversable[Int]).map(x => x))
    test[sci.LinearSeq[?]       ]((sci.LinearSeq(1):        sc.GenTraversable[Int]).map(x => x))
    test[sci.LinearSeq[?]       ]((sci.LinearSeq(1):        sc.Seq[Int]           ).map(x => x))
    test[sci.List[?]            ]((sci.List(1):             sc.GenTraversable[Int]).map(x => x))
    test[sci.List[?]            ]((sci.List(1):             sc.Seq[Int]           ).map(x => x))
    test[sci.Stream[?]          ]((sci.Stream(1):           sc.GenTraversable[Int]).map(x => x))
    test[sci.Stream[?]          ]((sci.Stream(1):           sc.Seq[Int]           ).map(x => x))
    //test[sci.Stack[?]           ]((sci.Stack(1):            sc.GenTraversable[Int]).map(x => x))
    //test[sci.Stack[?]           ]((sci.Stack(1):            sc.Seq[Int]           ).map(x => x))
    //test[sci.Queue[?]           ]((sci.Queue(1):            sc.GenTraversable[Int]).map(x => x))
    //test[sci.Queue[?]           ]((sci.Queue(1):            sc.Seq[Int]           ).map(x => x))
    test[sci.IndexedSeq[?]      ]((sci.IndexedSeq(1):       sc.GenTraversable[Int]).map(x => x))
    test[sci.IndexedSeq[?]      ]((sci.IndexedSeq(1):       sc.Seq[Int]           ).map(x => x))
    test[sci.Vector[?]          ]((sci.Vector(1):           sc.GenTraversable[Int]).map(x => x))
    test[sci.Vector[?]          ]((sci.Vector(1):           sc.Seq[Int]           ).map(x => x))
    test[sci.Seq[?]             ]((sci.Seq(1):              sc.GenTraversable[Int]).map(x => x))
    test[sci.Seq[?]             ]((sci.Seq(1):              sc.Seq[Int]           ).map(x => x))
    test[sci.Set[?]             ]((sci.Set(1):              sc.GenTraversable[Int]).map(x => x))
    test[sci.Set[?]             ]((sci.Set(1):              sc.Set[Int]           ).map(x => x))
    test[sci.ListSet[?]         ]((sci.ListSet(1):          sc.GenTraversable[Int]).map(x => x))
    test[sci.ListSet[?]         ]((sci.ListSet(1):          sc.Set[Int]           ).map(x => x))
    test[sci.HashSet[?]         ]((sci.HashSet(1):          sc.GenTraversable[Int]).map(x => x))
    test[sci.HashSet[?]         ]((sci.HashSet(1):          sc.Set[Int]           ).map(x => x))

    // These go through `GenMap.canBuildFrom`. There is no simple fix for Map like there is for Set.
    // A Map does not provide access to its companion object at runtime. (The `companion` field
    // points to an inherited `GenericCompanion`, not the actual companion object). Therefore, the
    // `MapCanBuildFrom` has no way to get the correct builder for the source type at runtime.
    //test[scm.Map[?, ?]          ]((scm.Map(1 -> 1):           sc.GenMap[Int, Int]).map(x => x)
    //test[scm.OpenHashMap[?, ?]  ]((scm.OpenHashMap(1 -> 1):   sc.GenMap[Int, Int]).map(x => x))
    //test[scm.LongMap[?]         ]((scm.LongMap(1L -> 1):      sc.GenMap[Long, Int]).map(x => x))
    //test[scm.ListMap[?, ?]      ]((scm.ListMap(1 -> 1):       sc.GenMap[Int, Int]).map(x => x))
    //test[scm.LinkedHashMap[?, ?]]((scm.LinkedHashMap(1 -> 1): sc.GenMap[Int, Int]).map(x => x))
    //test[scm.HashMap[?, ?]      ]((scm.HashMap(1 -> 1):       sc.GenMap[Int, Int]).map(x => x))
    //test[sci.Map[?, ?]          ]((sci.Map(1 -> 1):           sc.GenMap[Int, Int]).map(x => x))
    //test[sci.ListMap[?, ?]      ]((sci.ListMap(1 -> 1):       sc.GenMap[Int, Int]).map(x => x))
    //test[sci.IntMap[?]          ]((sci.IntMap(1 -> 1):        sc.GenMap[Int, Int]).map(x => x))
    //test[sci.LongMap[?]         ]((sci.LongMap(1L -> 1):      sc.GenMap[Long, Int]).map(x => x))
    //test[sci.HashMap[?, ?]      ]((sci.HashMap(1 -> 1):       sc.GenMap[Int, Int]).map(x => x))
    //test[sci.SortedMap[?, ?]    ]((sci.SortedMap(1 -> 1):     sc.GenMap[Int, Int]).map(x => x))
    //test[sci.TreeMap[?, ?]      ]((sci.TreeMap(1 -> 1):       sc.GenMap[Int, Int]).map(x => x))
    //test[scc.TrieMap[?, ?]      ]((scc.TrieMap(1 -> 1):       sc.GenMap[Int, Int]).map(x => x))

    // These cannot be expected to work. The static type information is lost, and `map` does not capture
    // a `ClassTag` of the result type, so there is no way for a `CanBuildFrom` to decide to build another
    // `BitSet` instead of a generic `Set` implementation:
    //test[scm.BitSet          ]((scm.BitSet(1):        sc.GenTraversable[Int]).map(x => x))
    //test[scm.BitSet          ]((scm.BitSet(1):        sc.Set[Int]).map(x => x))

    // These also require a `ClassTag`:
    //test[scm.UnrolledBuffer[?]]((scm.UnrolledBuffer(1): sc.GenTraversable[Int]).map(x => x))
    //test[scm.UnrolledBuffer[?]]((scm.UnrolledBuffer(1): sc.Seq[Int]).map(x => x))

    // The situation is similar for sorted collection. They require an implicit `Ordering` which cannot
    // be captured at runtime by a `CanBuildFrom` when the static type has been lost:
    //test[sc.SortedMap[?, ?]  ]((sc.SortedMap(1 -> 1):  sc.GenTraversable[(Int, Int)]).map(x => x))
    //test[sc.SortedMap[?, ?]  ]((sc.SortedMap(1 -> 1):  sc.GenMap[Int, Int]).map(x => x))
    //test[sc.SortedSet[?]     ]((sc.SortedSet(1):       sc.GenTraversable[Int]).map(x => x))
    //test[sc.SortedSet[?]     ]((sc.SortedSet(1):       sc.Set[Int]).map(x => x))
    //test[scm.SortedSet[?]    ]((scm.SortedSet(1):      sc.GenTraversable[Int]).map(x => x))
    //test[scm.SortedSet[?]    ]((scm.SortedSet(1):      sc.Set[Int]).map(x => x))
    //test[scm.TreeSet[?]      ]((scm.TreeSet(1):        sc.GenTraversable[Int]).map(x => x))
    //test[scm.TreeSet[?]      ]((scm.TreeSet(1):        sc.Set[Int]).map(x => x))
    //test[scm.TreeMap[?, ?]   ]((scm.TreeMap(1 -> 1):   sc.GenTraversable[(Int, Int)]).map(x => x))
    //test[scm.TreeMap[?, ?]   ]((scm.TreeMap(1 -> 1):   sc.GenMap[Int, Int]).map(x => x))
    //test[scm.SortedMap[?, ?] ]((scm.SortedMap(1 -> 1): sc.GenTraversable[(Int, Int)]).map(x => x))
    //test[scm.SortedMap[?, ?] ]((scm.SortedMap(1 -> 1): sc.GenMap[Int, Int]).map(x => x))

    // Maps do not map to maps when seen as GenTraversable. This would require knowledge that `map`
    // returns a `Tuple2`, which is not available dynamically:
    //test[sc.GenMap[?, ?]     ]((sc.GenMap(1 -> 1):    sc.GenTraversable[(Int, Int)]).map(x => x))
    //test[sc.Map[?, ?]        ]((sc.Map(1 -> 1):       sc.GenTraversable[(Int, Int)]).map(x => x))
   }
}
