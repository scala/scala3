package json.value.gen

import json.value.*
import org.scalacheck.*
import json.value.spec.{IsArrayOf, JsObjSpec, SchemaSpec}

/**
 *
 */
extension[T <: Json[T]] (gen: Gen[T]) {
  def retryUntil(spec:SchemaSpec[T]):Gen[T] = gen.retryUntil(it => spec.validateAll(it).isEmpty,100000)
  def retryUntilNot(spec:SchemaSpec[T]):Gen[T]  = gen.retryUntil(spec.validateAll(_).nonEmpty,10000)
  def partition(spec:SchemaSpec[T]):(Gen[T],Gen[T])   = (retryUntil(spec),retryUntilNot(spec))

  def retryUntil(spec:SchemaSpec[T], maxTries:Int):Gen[T] = gen.retryUntil(spec.validateAll(_).isEmpty, maxTries)
  def retryUntilNot(spec:SchemaSpec[T], maxTries:Int):Gen[T] = gen.retryUntil(spec.validateAll(_).nonEmpty, maxTries)
  def partition(spec:SchemaSpec[T], maxTries:Int):(Gen[T],Gen[T]) = (retryUntil(spec,maxTries), retryUntilNot(spec,maxTries))
  
}

/**
 * extension methods over Gen[JsObj]
 */
extension (gen: Gen[JsObj]) {
  /**
 *
 * @param opt list of optional keys
 * @return a new generator that removes with the same probability all the possible compinations of opt fields
 */
  def withOptKeys(opt: String*):Gen[JsObj] =
    //appended seq empty so that no keys removed is also a possible outcome
    val xs = allCombinations(opt) appended Seq.empty
    for
      keys <- Gen.oneOf(xs)
      obj <- gen
    yield
      obj.removedAll(keys)

  def withNullValues(nullable: String*): Gen[JsObj] =
    def updatedAllNull(o:JsObj,keys:Seq[String]):JsObj =
      if keys.isEmpty then o
      else updatedAllNull(o.updated(keys.head,JsNull),keys.tail)
    //appended seq empty so that no keys associated to null is also a possible outcome
    val xs = allCombinations (nullable).appended(Seq.empty)
    for
      keys <- Gen.oneOf (xs)
      obj <- gen
    yield updatedAllNull(obj,keys)

  def concat(other: Gen[JsObj], rest: Gen[JsObj]*): Gen[JsObj] = concatGens(gen, other, rest: _*)

  def updated(key:String,other:Gen[JsValue]):Gen[JsObj] = gen.concat(JsObjGen((key,other)))
}

extension (gen: Gen[JsArray]) {

  def distinct: Gen[JsArray] =
    for
      arr <- gen
    yield JsArray(arr.seq.distinct)

  def appendedAll(other:Gen[JsArray]):Gen[JsArray] =
    for
      a <- gen
      b <- other
    yield a.appendedAll(b)


  def prependedAll(other: Gen[JsArray]): Gen[JsArray] =
    for
      a <- gen
      b <- other
    yield a.prependedAll(b)

  def appended(other: Gen[JsValue]): Gen[JsArray] =
    for
      a <- gen
      b <- other
    yield a.appended(b)

  def prepended(other: Gen[JsValue]): Gen[JsArray] =
    for
      a <- gen
      b <- other
    yield a.prepended(b)
}


private[gen] def allCombinations(ys: Seq[String]): Seq[Seq[String]] =
    def rec(xs: Seq[String], n: Int, acc: Seq[Seq[String]]): Seq[Seq[String]]=
      if n == 0 then acc
      else acc ++ xs.combinations(n) ++ rec(xs, n - 1, acc)
    rec(ys,ys.size,Seq.empty)



@scala.annotation.tailrec
private[gen] def genFromPairs[T <: Json[T]](acc: Gen[T],
                                            pairs: Seq[(JsPath, Gen[JsValue])]
                                           ): Gen[T] =
    if pairs.isEmpty then acc
    else
      val headGenerated =
        for
          obj <- acc
          (key, gen) = pairs.head
          value <- gen
        yield obj updated (key, value)
      genFromPairs(headGenerated, pairs.tail)


@scala.annotation.tailrec
private[gen] def concatGens(a: Gen[JsObj],
                            b: Gen[JsObj],
                            rest: Gen[JsObj]*
                           ): Gen[JsObj] =
    def concatTwo(a: Gen[JsObj], b: Gen[JsObj]): Gen[JsObj] =
      for
        av <- a
        bv <- b
      yield av concat bv

    val c = concatTwo(a, b)
    if rest.isEmpty then c
    else concatGens(c, rest.head, rest.tail: _*)


