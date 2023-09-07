package dotty.tools.dotc.util

object lrutest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val bits = new SixteenNibbles(0L)               //> bits  : dotty.tools.dotc.util.SixteenNibbles = SixteenNibbles(0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  bits.updated(1, 3)                              //> res0: dotty.tools.dotc.util.SixteenNibbles = SixteenNibbles(0, 3, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  LRUCache.initialRing                            //> res1: dotty.tools.dotc.util.SixteenNibbles = SixteenNibbles(1, 2, 3, 4, 5, 6
                                                  //| , 7, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  val cache = new LRUCache[String, String]        //> cache  : dotty.tools.dotc.util.LRUCache[String,String] = LRUCache()
  cache lookup "hi"                               //> res2: String = null
  cache enter ("hi", "x")
	cache.indices.take(10).toList             //> res3: List[Int] = List(7, 0, 1, 2, 3, 4, 5, 6, 7, 0)
	cache.last                                //> res4: Int = 6
	cache lookup "hi"                         //> res5: String = x
	cache.indices.take(10).toList             //> res6: List[Int] = List(7, 0, 1, 2, 3, 4, 5, 6, 7, 0)

	for (i <- 1 to 10) {
	  if (cache.lookup(i.toString) == null)
		  cache.enter(i.toString, i.toString)
	}

  cache.indices.take(10).toList                   //> res7: List[Int] = List(5, 6, 7, 0, 1, 2, 3, 4, 5, 6)
	cache                                     //> res8: dotty.tools.dotc.util.LRUCache[String,String] = LRUCache(10 -> 10, 9 -
                                                  //| > 9, 8 -> 8, 7 -> 7, 6 -> 6, 5 -> 5, 4 -> 4, 3 -> 3)
  cache                                           //> res9: dotty.tools.dotc.util.LRUCache[String,String] = LRUCache(10 -> 10, 9 -
                                                  //| > 9, 8 -> 8, 7 -> 7, 6 -> 6, 5 -> 5, 4 -> 4, 3 -> 3)
  cache.lookup("7")                               //> res10: String = 7
  cache.indices.take(10).toList                   //> res11: List[Int] = List(0, 5, 6, 7, 1, 2, 3, 4, 0, 5)
	cache.keysIterator.toList                 //> res12: List[String] = List(7, 10, 9, 8, 6, 5, 4, 3)
  cache.lookup("10")                              //> res13: String = 10
  cache.lookup("5")                               //> res14: String = 5
  cache                                           //> res15: dotty.tools.dotc.util.LRUCache[String,String] = LRUCache(5 -> 5, 10 -
                                                  //| > 10, 7 -> 7, 9 -> 9, 8 -> 8, 6 -> 6, 4 -> 4, 3 -> 3)
  cache.lookup("11")                              //> res16: String = null
  cache.enter("11", "!!")
  cache                                           //> res17: dotty.tools.dotc.util.LRUCache[String,String] = LRUCache(11 -> !!, 5
                                                  //| -> 5, 10 -> 10, 7 -> 7, 9 -> 9, 8 -> 8, 6 -> 6, 4 -> 4)
}