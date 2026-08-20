/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package concurrent

import scala.language.`2.13`
import language.experimental.captureChecking

import java.util.concurrent.atomic._
import scala.{unchecked => uc}
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap.RemovalPolicy
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{List, Nil}
import scala.collection.mutable.GrowableBuilder
import scala.util.Try
import scala.util.hashing.Hashing

private[collection] final class INode[K, V](bn: MainNode[K, V] | Null, g: Gen, equiv: Equiv[K]) extends INodeBase[K, V](g) {
  import INodeBase._

  WRITE(bn)

  /** Initializes a new INode with the specified generation and equivalence relation, setting its main node to null.
   *
   *  @param g the generation to use for this INode
   *  @param equiv the equivalence relation for comparing keys
   */
  def this(g: Gen, equiv: Equiv[K]) = this(null, g, equiv)

  /** Atomically writes the specified main node value to this INode using a low-level atomic reference update.
   *
   *  @param nval the new main node value to set
   */
  def WRITE(nval: MainNode[K, V] | Null) = INodeBase.updater.set(this, nval)

  /** Atomically compares and sets the main node of this INode from the expected old value to the new value.
   *
   *  @param old the expected current main node value
   *  @param n the new main node value to set
   */
  def CAS(old: MainNode[K, V], n: MainNode[K, V]) = INodeBase.updater.compareAndSet(this, old, n)

  /** Reads the main node of this INode using the GCAS (Generalized Compare-And-Set) protocol.
   *
   *  @param ct the TrieMap instance
   *  @return the current main node, or null if not available
   */
  def gcasRead(ct: TrieMap[K, V]): MainNode[K, V] | Null = GCAS_READ(ct)

  /** Reads the main node of this INode using the GCAS (Generalized Compare-And-Set) protocol.
   *
   *  @param ct the TrieMap instance
   *  @return the current main node, or null if not available
   */
  def GCAS_READ(ct: TrieMap[K, V]): MainNode[K, V] | Null = {
    val m = /*READ*/mainnode
    val prevval: MainNode[K, V] | Null = /*READ*/m.prev
    if (prevval eq null) m
    else GCAS_Complete(m, ct)
  }

  @tailrec private def GCAS_Complete(m: MainNode[K, V] | Null, ct: TrieMap[K, V]): MainNode[K, V] | Null = if (m eq null) null else {
    // complete the GCAS
    val prev: MainNode[K, V] | Null = /*READ*/m.prev
    val ctr = ct.readRoot(abort = true)

    prev match {
      case null =>
        m
      case fn: FailedNode[?, ?] => // try to commit to previous value
        if (CAS(m, fn.prev)) fn.prev
        else GCAS_Complete(/*READ*/mainnode, ct)
      case vn: MainNode[?, ?] =>
        // Assume that you've read the root from the generation G.
        // Assume that the snapshot algorithm is correct.
        // ==> you can only reach nodes in generations <= G.
        // ==> `gen` is <= G.
        // We know that `ctr.gen` is >= G.
        // ==> if `ctr.gen` = `gen` then they are both equal to G.
        // ==> otherwise, we know that either `ctr.gen` > G, `gen` < G,
        //     or both
        if ((ctr.gen eq gen) && ct.nonReadOnly) {
          // try to commit
          if (m.CAS_PREV(prev, null)) m
          else GCAS_Complete(m, ct)
        } else {
          // try to abort
          // we know `vn eq prev` and `prev ne null`
          m.CAS_PREV(prev, new FailedNode(vn))
          GCAS_Complete(/*READ*/mainnode, ct)
        }
    }
  }

  /** Atomically updates the main node from the expected old value to the new value using the GCAS protocol, ensuring atomicity across concurrent operations.
   *
   *  @param old the expected current main node value
   *  @param n the new main node value to set
   *  @param ct the TrieMap instance
   *  @return true if the update was successful, false otherwise
   */
  def GCAS(old: MainNode[K, V], n: MainNode[K, V], ct: TrieMap[K, V]): Boolean = {
    n.WRITE_PREV(old)
    if (CAS(old, n)) {
      GCAS_Complete(n, ct)
      /*READ*/n.prev eq null
    } else false
  }

  private def equal(k1: K, k2: K, ct: TrieMap[K, V]) = ct.equality.equiv(k1, k2)

  private def inode(cn: MainNode[K, V]) = {
    val nin = new INode[K, V](gen, equiv)
    nin.WRITE(cn)
    nin
  }

  /** Creates a deep copy of this INode with the specified generation, used for snapshot operations.
   *
   *  @param ngen the new generation to use for the copied INode
   *  @param ct the TrieMap instance
   */
  def copyToGen(ngen: Gen, ct: TrieMap[K, V]) = {
    val nin = new INode[K, V](ngen, equiv)
    val main = GCAS_READ(ct)
    nin.WRITE(main)
    nin
  }

  /** Inserts a key value pair, overwriting the old pair if the keys match.
   *
   *  @param k the key to insert
   *  @param v the value to associate with `k`
   *  @param hc the hashcode of `k`
   *  @param lev the current level in the trie (in bits, increments of 5)
   *  @param parent the parent i-node, or null if this is the root
   *  @param startgen the generation of the root when the operation started
   *  @param ct the TrieMap instance
   *  @return        true if the insertion was committed, false if a retry is needed
   */
  @tailrec def rec_insert(k: K, v: V, hc: Int, lev: Int, parent: INode[K, V] | Null, startgen: Gen, ct: TrieMap[K, V]): Boolean = {
    val m = GCAS_READ(ct) // use -Yinline!

    m match {
      case cn: CNode[K, V] => // 1) a multiway node
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        val mask = flag - 1
        val pos = Integer.bitCount(bmp & mask)
        if ((bmp & flag) != 0) {
          // 1a) insert below
          cn.array(pos) match {
            case in: INode[K, V] @uc =>
              if (startgen eq in.gen) in.rec_insert(k, v, hc, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct)) rec_insert(k, v, hc, lev, parent, startgen, ct)
                else false
              }
            case sn: SNode[K, V] @uc =>
              if (sn.hc == hc && equal(sn.k, k, ct)) GCAS(cn, cn.updatedAt(pos, new SNode(sn.k, v, hc), gen), ct)
              else {
                val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen, equiv)), gen)
                GCAS(cn, nn, ct)
              }
            case basicNode => throw new MatchError(basicNode)
          }
        } else {
          val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
          val ncnode = rn.insertedAt(pos, flag, k, v, hc, gen)
          GCAS(cn, ncnode, ct)
        }
      case tn: TNode[K, V] =>
        clean(parent.nn, ct, lev - 5)
        false
      case ln: LNode[K, V] => // 3) an l-node
        val nn = ln.inserted(k, v)
        GCAS(ln, nn, ct)
      case mainNode => throw new MatchError(mainNode)
    }
  }



  /** Inserts a new key value pair, given that a specific condition is met.
   *
   *  @param cond KEY_PRESENT_OR_ABSENT - don't care if the key was there, insert or overwrite
   *              KEY_ABSENT - key wasn't there, insert only, do not overwrite
   *              KEY_PRESENT - key was there, overwrite only, do not insert
   *              other value `v` - only overwrite if the current value is this
   *  @param fullEquals whether to use reference or full equals when comparing `v` to the current value
   *  @param hc the hashcode of `k`
   *
   *  @param k the key to insert
   *  @param v the value to associate with `k`
   *  @param lev the current level in the trie (in bits, increments of 5)
   *  @param parent the parent i-node, or null if this is the root
   *  @param startgen the generation of the root when the operation started
   *  @param ct the TrieMap instance
   *  @return     null if unsuccessful, `Option[V]` otherwise (indicating the previous value bound to the key)
   */
  @tailrec def rec_insertif(k: K, v: V, hc: Int, cond: AnyRef, fullEquals: Boolean, lev: Int, parent: INode[K, V] | Null, startgen: Gen, ct: TrieMap[K, V]): Option[V] | Null = {
    val m = GCAS_READ(ct)  // use -Yinline!

    m match {
      case cn: CNode[K, V] => // 1) a multiway node
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        val mask = flag - 1
        val pos = Integer.bitCount(bmp & mask)
        if ((bmp & flag) != 0) {
          // 1a) insert below
          cn.array(pos) match {
            case in: INode[K, V] @uc =>
              if (startgen eq in.gen) in.rec_insertif(k, v, hc, cond, fullEquals, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct)) rec_insertif(k, v, hc, cond, fullEquals, lev, parent, startgen, ct)
                else null
              }
            case sn: SNode[K, V] @uc => cond match {
              case INode.KEY_PRESENT_OR_ABSENT =>
                if (sn.hc == hc && equal(sn.k, k, ct)) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(sn.k, v, hc), gen), ct)) Some(sn.v) else null
                } else {
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                  val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen, equiv)), gen)
                  if (GCAS(cn, nn, ct)) None
                  else null
                }
              case INode.KEY_ABSENT =>
                if (sn.hc == hc && equal(sn.k, k, ct)) Some(sn.v)
                else {
                  val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
                  val nn = rn.updatedAt(pos, inode(CNode.dual(sn, sn.hc, new SNode(k, v, hc), hc, lev + 5, gen, equiv)), gen)
                  if (GCAS(cn, nn, ct)) None
                  else null
                }
              case INode.KEY_PRESENT =>
                if (sn.hc == hc && equal(sn.k, k, ct)) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)) Some(sn.v) else null
                } else None
              case otherv =>
                if (sn.hc == hc && equal(sn.k, k, ct) && (if (fullEquals) sn.v == otherv else sn.v.asInstanceOf[AnyRef] eq otherv)) {
                  if (GCAS(cn, cn.updatedAt(pos, new SNode(k, v, hc), gen), ct)) Some(sn.v) else null
                } else None
            }
            case basicNode => throw new MatchError(basicNode)
          }
        } else cond match {
          case INode.KEY_PRESENT_OR_ABSENT | INode.KEY_ABSENT =>
            val rn = if (cn.gen eq gen) cn else cn.renewed(gen, ct)
            val ncnode = rn.insertedAt(pos, flag, k, v, hc, gen)
            if (GCAS(cn, ncnode, ct)) None else null
          case INode.KEY_PRESENT => None
          case otherv => None
        }
      case sn: TNode[K, V] =>
        clean(parent.nn, ct, lev - 5)
        null
      case ln: LNode[K, V] => // 3) an l-node
        def insertln() = {
          val nn = ln.inserted(k, v)
          GCAS(ln, nn, ct)
        }
        cond match {
          case INode.KEY_PRESENT_OR_ABSENT =>
            val optv = ln.get(k)
            if (insertln()) optv else null
          case INode.KEY_ABSENT =>
            ln.get(k) match {
              case None => if (insertln()) None else null
              case optv => optv
            }
          case INode.KEY_PRESENT =>
            ln.get(k) match {
              case Some(v0) => if (insertln()) Some(v0) else null
              case None => None
            }
          case otherv =>
            ln.get(k) match {
              case Some(v0) if (if (fullEquals) v0 == otherv else v0.asInstanceOf[AnyRef] eq otherv) =>
                if (insertln()) Some(otherv.asInstanceOf[V]) else null
              case _ => None
            }
        }
      case mainNode => throw new MatchError(mainNode)
    }
  }

  /** Looks up the value associated with the key.
   *
   *  @param hc        the hashcode of `k`
   *
   *  @param k the key to look up
   *  @param lev the current level in the trie (in bits, increments of 5)
   *  @param parent the parent i-node, or null if this is the root
   *  @param startgen the generation of the root when the operation started
   *  @param ct the TrieMap instance
   *  @return          NO_SUCH_ELEMENT_SENTINEL if no value has been found, RESTART if the operation wasn't successful,
   *                   or any other value otherwise
   */
  @tailrec def rec_lookup(k: K, hc: Int, lev: Int, parent: INode[K, V] | Null, startgen: Gen, ct: TrieMap[K, V]): AnyRef = {
    val m = GCAS_READ(ct) // use -Yinline!

    m match {
      case cn: CNode[K, V] => // 1) a multinode
        val idx = (hc >>> lev) & 0x1f
        val flag = 1 << idx
        val bmp = cn.bitmap
        if ((bmp & flag) == 0) NO_SUCH_ELEMENT_SENTINEL // 1a) bitmap shows no binding
        else { // 1b) bitmap contains a value - descend
          val pos = if (bmp == 0xffffffff) idx else Integer.bitCount(bmp & (flag - 1))
          val sub = cn.array(pos)
          sub match {
            case in: INode[K, V] @uc =>
              if (ct.isReadOnly || (startgen eq in.gen)) in.rec_lookup(k, hc, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct)) rec_lookup(k, hc, lev, parent, startgen, ct)
                else RESTART
              }
            case sn: SNode[K, V] @uc => // 2) singleton node
              if (sn.hc == hc && equal(sn.k, k, ct)) sn.v.asInstanceOf[AnyRef]
              else NO_SUCH_ELEMENT_SENTINEL
            case basicNode => throw new MatchError(basicNode)
          }
        }
      case tn: TNode[?, ?] => // 3) non-live node
        def cleanReadOnly(tn: TNode[K, V]) = if (ct.nonReadOnly) {
          clean(parent.nn, ct, lev - 5)
          RESTART
        } else {
          if (tn.hc == hc && tn.k == k) tn.v.asInstanceOf[AnyRef]
          else NO_SUCH_ELEMENT_SENTINEL
        }
        cleanReadOnly(tn)
      case ln: LNode[K, V] => // 5) an l-node
        ln.get(k).asInstanceOf[Option[AnyRef]].getOrElse(NO_SUCH_ELEMENT_SENTINEL)
      case mainNode => throw new MatchError(mainNode)
    }
  }

  /** Removes the key associated with the given value.
   *
   *  @param hc            the hashcode of `k`
   *
   *  @param removalPolicy policy deciding whether to remove `k` based on `v` and the
   *                       current value associated with `k` (Always, FullEquals, or ReferenceEq)
   *
   *  @param k the key to remove
   *  @param v the value to compare against when `removalPolicy` requires it
   *  @param lev the current level in the trie (in bits, increments of 5)
   *  @param parent the parent i-node, or null if this is the root
   *  @param startgen the generation of the root when the operation started
   *  @param ct the TrieMap instance
   *  @return              null if not successful, an Option[V] indicating the previous value otherwise
   */
  def rec_remove(
    k: K,
    v: V,
    removalPolicy: Int,
    hc: Int,
    lev: Int,
    parent: INode[K, V] | Null,
    startgen: Gen,
    ct: TrieMap[K, V]): Option[V] | Null = {

    GCAS_READ(ct) match {
      case cn: CNode[K, V] =>
        val idx = (hc >>> lev) & 0x1f
        val bmp = cn.bitmap
        val flag = 1 << idx
        if ((bmp & flag) == 0) None
        else {
          val pos = Integer.bitCount(bmp & (flag - 1))
          val sub = cn.array(pos)
          val res = sub match {
            case in: INode[K, V] @uc =>
              if (startgen eq in.gen) in.rec_remove(k, v, removalPolicy, hc, lev + 5, this, startgen, ct)
              else {
                if (GCAS(cn, cn.renewed(startgen, ct), ct)) rec_remove(k, v, removalPolicy, hc, lev, parent, startgen, ct)
                else null
              }
            case sn: SNode[K, V] @uc =>
              if (sn.hc == hc && equal(sn.k, k, ct) && RemovalPolicy.shouldRemove(removalPolicy)(sn.v, v)) {
                val ncn = cn.removedAt(pos, flag, gen).toContracted(lev)
                if (GCAS(cn, ncn, ct)) Some(sn.v) else null
              } else None
            case basicNode => throw new MatchError(basicNode)
          }

          if (res == None || (res eq null)) res
          else {
            @tailrec def cleanParent(nonlive: AnyRef | Null): Unit = {
              val cn = parent.nn.GCAS_READ(ct)
              cn match {
                case cn: CNode[K, V] =>
                  val idx = (hc >>> (lev - 5)) & 0x1f
                  val bmp = cn.bitmap
                  val flag = 1 << idx
                  if ((bmp & flag) == 0) {} // somebody already removed this i-node, we're done
                  else {
                    val pos = Integer.bitCount(bmp & (flag - 1))
                    val sub = cn.array(pos)
                    if (sub eq this) (nonlive: @uc) match {
                      case tn: TNode[K, V] @uc =>
                        val ncn = cn.updatedAt(pos, tn.copyUntombed, gen).toContracted(lev - 5)
                        if (!parent.nn.GCAS(cn, ncn, ct))
                          if (ct.readRoot().gen == startgen) cleanParent(nonlive)
                    }
                  }
                case _ => // parent is no longer a cnode, we're done
              }
            }

            if (parent ne null) { // never tomb at root
              val n = GCAS_READ(ct)
              if (n.isInstanceOf[TNode[?, ?]])
                cleanParent(n)
            }

            res
          }
        }
      case tn: TNode[K, V] =>
        clean(parent.nn, ct, lev - 5)
        null
      case ln: LNode[K, V] =>
        if (removalPolicy == RemovalPolicy.Always) {
          val optv = ln.get(k)
          val nn = ln.removed(k, ct)
          if (GCAS(ln, nn, ct)) optv else null
        } else ln.get(k) match {
          case optv @ Some(v0) if RemovalPolicy.shouldRemove(removalPolicy)(v, v0) =>
            val nn = ln.removed(k, ct)
            if (GCAS(ln, nn, ct)) optv else null
          case _ => None
        }
      case mainNode => throw new MatchError(mainNode)
    }
  }

  private def clean(nd: INode[K, V], ct: TrieMap[K, V], lev: Int): Unit = {
    val m = nd.GCAS_READ(ct)
    m match {
      case cn: CNode[K, V] => nd.GCAS(cn, cn.toCompressed(ct, lev, gen), ct)
      case _ =>
    }
  }

  /** Checks if this INode’s main node, as read via GCAS, is null.
   *
   *  @param ct the TrieMap instance
   */
  def isNullInode(ct: TrieMap[K, V]) = GCAS_READ(ct) eq null

  /** Returns the cached size of this INode.
   *
   *  @param ct the TrieMap instance
   */
  def cachedSize(ct: TrieMap[K, V]): Int =
    GCAS_READ(ct).nn.cachedSize(ct)

  /** Returns the known size of this INode.
   *
   *  @param ct the TrieMap instance
   */
  def knownSize(ct: TrieMap[K, V]): Int =
    GCAS_READ(ct).nn.knownSize()

  /* this is a quiescent method! */
  /** Returns a string representation of this INode.
   *
   *  @param lev the current level in the trie
   */
  def string(lev: Int): String = {
    val spaces = "  " * lev
    val rhs = mainnode match {
      case null => "<null>"
      case tn: TNode[?, ?] => s"TNode(${tn.k}, ${tn.v}, ${tn.hc}, !)"
      case cn: CNode[?, ?] => cn.string(lev)
      case ln: LNode[?, ?] => ln.string(lev)
      case x => s"<elem: $x>"
    }
    s"${spaces}INode -> $rhs"
  }

}


private[concurrent] object INode {
  ////////////////////////////////////////////////////////////////////////////////////////////////////
  // Arguments for `cond` argument in TrieMap#rec_insertif
  ////////////////////////////////////////////////////////////////////////////////////////////////////
  /** A condition value indicating that the key must be present for the operation to succeed.
   */
  final val KEY_PRESENT = new AnyRef
  /** A condition value indicating that the key must be absent for the operation to succeed.
   */
  final val KEY_ABSENT = new AnyRef
  /** A condition value indicating that the operation should succeed regardless of whether the key is present or absent.
   */
  final val KEY_PRESENT_OR_ABSENT = new AnyRef

  /** Creates a new root node for a TrieMap.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param equiv the equivalence relation for comparing keys
   */
  def newRootNode[K, V](equiv: Equiv[K]) = {
    val gen = new Gen
    val cn = new CNode[K, V](0, new Array(0), gen)
    new INode[K, V](cn, gen, equiv)
  }
}


private[concurrent] final class FailedNode[K, V](p: MainNode[K, V]) extends MainNode[K, V] {
  WRITE_PREV(p)

  /** Throws `UnsupportedOperationException` because FailedNode is an internal transient state and does not support string representation.
   *
   *  @param lev the current level in the trie
   *  @throws UnsupportedOperationException always
   */
  def string(lev: Int): Nothing = throw new UnsupportedOperationException

  /** Throws `UnsupportedOperationException` because FailedNode is an internal transient state and does not support size queries.
   *
   *  @param ct unused
   *  @throws UnsupportedOperationException always
   */
  def cachedSize(ct: AnyRef): Int = throw new UnsupportedOperationException

  /** Always throws `UnsupportedOperationException`; FailedNode does not support size queries.
   *
   *  @throws UnsupportedOperationException always
   */
  def knownSize: Int = throw new UnsupportedOperationException

  /** Returns a string representation of this FailedNode.
   */
  override def toString(): String = s"FailedNode($p)"
}


private[concurrent] trait KVNode[K, V] {
  /** Returns the key-value pair stored in this node.
   */
  def kvPair: (K, V)
}


private[collection] final class SNode[K, V](final val k: K, final val v: V, final val hc: Int)
  extends BasicNode with KVNode[K, V] {
  /** Creates a copy of this SNode.
   */
  def copy = new SNode(k, v, hc)
  /** Creates a tombed copy of this SNode, marking it for removal in the trie structure.
   */
  def copyTombed = new TNode(k, v, hc)
  /** Creates an untombed copy of this SNode, restoring it to a live state in the trie.
   */
  def copyUntombed = new SNode(k, v, hc)
  /** Returns the key-value pair stored in this node.
   */
  def kvPair = (k, v)
  /** Returns a string representation of this SNode.
   *
   *  @param lev the current level in the trie
   *  @return a string representation
   */
  def string(lev: Int): String = ("  " * lev) + s"SNode($k, $v, ${hc.toHexString})"
}

// Tomb Node, used to ensure proper ordering during removals
private[collection] final class TNode[K, V](final val k: K, final val v: V, final val hc: Int)
  extends MainNode[K, V] with KVNode[K, V] {
  /** Creates a copy of this TNode.
   */
  def copy = new TNode(k, v, hc)
  /** Returns this TNode, which is already tombed.
   */
  def copyTombed = new TNode(k, v, hc)
  /** Creates an untombed SNode copy of this TNode, restoring it to a live state.
   */
  def copyUntombed = new SNode(k, v, hc)
  /** Returns the key-value pair stored in this node.
   */
  def kvPair = (k, v)
  /** Returns the cached size of this TNode.
   *
   *  @param ct unused
   *  @return the cached size
   */
  def cachedSize(ct: AnyRef): Int = 1
  /** Returns the known size of this TNode.
   */
  def knownSize: Int = 1
  /** Returns a string representation of this TNode.
   *
   *  @param lev the current level in the trie
   *  @return a string representation
   */
  def string(lev: Int): String = ("  " * lev) + s"TNode($k, $v, ${hc.toHexString}, !)"
}

// List Node, leaf node that handles hash collisions
private[collection] final class LNode[K, V](val entries: List[(K, V)], equiv: Equiv[K])
  extends MainNode[K, V] {

  /** Initializes a new LNode with a single key-value pair to handle hash collisions.
   *
   *  @param k the key
   *  @param v the value
   *  @param equiv the equivalence relation for comparing keys
   */
  def this(k: K, v: V, equiv: Equiv[K]) = this((k -> v) :: Nil, equiv)

  /** Initializes a new LNode with two key-value pairs, deduplicating keys if they are equivalent per `equiv`.
   *
   *  @param k1 the first key
   *  @param v1 the first value
   *  @param k2 the second key
   *  @param v2 the second value
   *  @param equiv the equivalence relation for comparing keys
   */
  def this(k1: K, v1: V, k2: K, v2: V, equiv: Equiv[K]) =
    this(if (equiv.equiv(k1, k2)) (k2 -> v2) :: Nil else (k1 -> v1) :: (k2 -> v2) :: Nil, equiv)

  /** Inserts or replaces a key-value pair in this LNode, handling hash collisions.
   *
   *  @param k the key to insert
   *  @param v the value to associate with the key
   *  @return the new LNode with the pair inserted
   */
  def inserted(k: K, v: V) = {
    var k0: K = k
    @tailrec
    def remove(elems: List[(K, V)], acc: List[(K, V)]): List[(K, V)] = {
      if (elems.isEmpty) acc
      else if (equiv.equiv(elems.head._1, k)) {
        k0 = elems.head._1
        acc ::: elems.tail
      } else remove(elems.tail, elems.head :: acc)
    }
    val e = remove(entries, Nil)
    new LNode((k0 -> v) :: e, equiv)
  }

  /** Removes the key-value pair with the specified key, returning a TNode if the LNode would contain only one pair.
   *
   *  @param k the key to remove
   *  @param ct the TrieMap instance
   *  @return the new main node after removal
   */
  def removed(k: K, ct: TrieMap[K, V]): MainNode[K, V] = {
    val updmap = entries.filterNot(entry => equiv.equiv(entry._1, k))
    if (updmap.sizeIs > 1) new LNode(updmap, equiv)
    else {
      val (k, v) = updmap.iterator.next()
      new TNode(k, v, ct.computeHash(k)) // create it tombed so that it gets compressed on subsequent accesses
    }
  }

  /** Returns the value associated with the specified key.
   *
   *  @param k the key to look up
   *  @return the value associated with the key, or None if not found
   */
  def get(k: K): Option[V] = entries.find(entry => equiv.equiv(entry._1, k)).map(_._2)

  /** Returns the cached size of this LNode.
   *
   *  @param ct unused
   *  @return the cached size
   */
  def cachedSize(ct: AnyRef): Int = entries.size

  /** Returns -1 to indicate that the size of this LNode is not precomputed due to the underlying list structure.
   */
  def knownSize: Int = -1 // shouldn't ever be empty, and the size of a list is not known

  /** Returns a string representation of this LNode.
   *
   *  @param lev the current level in the trie
   *  @return a string representation
   */
  def string(lev: Int): String = (" " * lev) + s"LNode(${entries.mkString(", ")})"

}

// Ctrie Node, contains bitmap and array of references to branch nodes
private[collection] final class CNode[K, V](val bitmap: Int, val array: Array[BasicNode], val gen: Gen) extends CNodeBase[K, V] {
  // this should only be called from within read-only snapshots
  /** Returns the cached size of this CNode, computing it if necessary in a thread-safe manner.
   *
   *  @param ct the TrieMap instance
   *  @return the cached size
   */
  def cachedSize(ct: AnyRef): Int = {
    val currsz = READ_SIZE()
    if (currsz != -1) currsz
    else {
      val sz = computeSize(ct.asInstanceOf[TrieMap[K, V]])
      while (READ_SIZE() == -1) CAS_SIZE(-1, sz)
      READ_SIZE()
    }
  }

  /** Returns the known size of this CNode.
   */
  def knownSize: Int = READ_SIZE() // this should only ever return -1 if unknown

  // lends itself towards being parallelizable by choosing
  // a random starting offset in the array
  // => if there are concurrent size computations, they start
  //    at different positions, so they are more likely to
  //    to be independent
  private def computeSize(ct: TrieMap[K, V]): Int = {
    var i = 0
    var sz = 0
    val offset =
      if (array.length > 0)
      //util.Random.nextInt(array.length) /* <-- benchmarks show that this causes observable contention */
        java.util.concurrent.ThreadLocalRandom.current.nextInt(0, array.length)
      else 0
    while (i < array.length) {
      val pos = (i + offset) % array.length
      array(pos) match {
        case sn: SNode[?, ?]     => sz += 1
        case in: INode[K, V] @uc => sz += in.cachedSize(ct)
        case basicNode           => throw new MatchError(basicNode)
      }
      i += 1
    }
    sz
  }

  /** Creates a new CNode with the specified node at the given position, preserving the bitmap and generation.
   *
   *  @param pos the position to update
   *  @param nn the new node to insert
   *  @param gen the generation to use for the new CNode
   */
  def updatedAt(pos: Int, nn: BasicNode, gen: Gen) = {
    val len = array.length
    val narr = new Array[BasicNode](len)
    Array.copy(array, 0, narr, 0, len)
    narr(pos) = nn
    new CNode[K, V](bitmap, narr, gen)
  }

  /** Creates a new CNode with the node at the specified position removed, updating the bitmap accordingly.
   *
   *  @param pos the position to remove
   *  @param flag the flag corresponding to the position
   *  @param gen the generation to use for the new CNode
   */
  def removedAt(pos: Int, flag: Int, gen: Gen) = {
    val arr = array
    val len = arr.length
    val narr = new Array[BasicNode](len - 1)
    Array.copy(arr, 0, narr, 0, pos)
    Array.copy(arr, pos + 1, narr, pos, len - pos - 1)
    new CNode[K, V](bitmap ^ flag, narr, gen)
  }

  /** Creates a new CNode with the specified key-value pair inserted at the given position, updating the bitmap to include the new flag.
   *
   *  @param pos the position to insert at
   *  @param flag the flag corresponding to the position
   *  @param k the key to insert
   *  @param v the value to associate with the key
   *  @param hc the hash code of the key
   *  @param gen the generation to use for the new CNode
   */
  def insertedAt(pos: Int, flag: Int, k: K, v: V, hc: Int, gen: Gen) = {
    val len = array.length
    val bmp = bitmap
    val narr = new Array[BasicNode](len + 1)
    Array.copy(array, 0, narr, 0, pos)
    narr(pos) = new SNode(k, v, hc)
    Array.copy(array, pos, narr, pos + 1, len - pos)
    new CNode[K, V](bmp | flag, narr, gen)
  }

  /** Returns a copy of this cnode such that all the i-nodes below it are copied
   *  to the specified generation `ngen`.
   *
   *  @param ngen the new generation to copy i-nodes into
   *  @param ct the TrieMap instance
   */
  def renewed(ngen: Gen, ct: TrieMap[K, V]) = {
    var i = 0
    val arr = array
    val len = arr.length
    val narr = new Array[BasicNode](len)
    while (i < len) {
      arr(i) match {
        case in: INode[K, V] @uc => narr(i) = in.copyToGen(ngen, ct)
        case bn: BasicNode       => narr(i) = bn
      }
      i += 1
    }
    new CNode[K, V](bitmap, narr, ngen)
  }

  private def resurrect(inode: INode[K, V], inodemain: AnyRef): BasicNode = inodemain match {
    case tn: TNode[?, ?] => tn.copyUntombed
    case _ => inode
  }

  /** Returns a contracted version of this CNode, optimizing the trie structure by reducing branching if possible.
   *
   *  @param lev the current level in the trie
   *  @return the contracted node, which may be a tombed leaf instead of a CNode
   */
  def toContracted(lev: Int): MainNode[K, V] = if (array.length == 1 && lev > 0) array(0) match {
    case sn: SNode[K, V] @uc => sn.copyTombed
    case _ => this
  } else this

  // - if the branching factor is 1 for this CNode, and the child
  //   is a tombed SNode, returns its tombed version
  // - otherwise, if there is at least one non-null node below,
  //   returns the version of this node with at least some null-inodes
  //   removed (those existing when the op began)
  // - if there are only null-i-nodes below, returns null
  /** Returns a compressed version of this CNode, removing null i-nodes and potentially contracting the result.
   *
   *  @param ct the TrieMap instance
   *  @param lev the current level in the trie
   *  @param gen the generation to use for the new node
   */
  def toCompressed(ct: TrieMap[K, V], lev: Int, gen: Gen) = {
    val bmp = bitmap
    var i = 0
    val arr = array
    val tmparray = new Array[BasicNode](arr.length)
    while (i < arr.length) { // construct new bitmap
      val sub = arr(i)
      sub match {
        case in: INode[K, V] @uc =>
          val inodemain = in.gcasRead(ct)
          assert(inodemain ne null)
          tmparray(i) = resurrect(in, inodemain)
        case sn: SNode[K, V] @uc =>
          tmparray(i) = sn
        case basicNode => throw new MatchError(basicNode)
      }
      i += 1
    }

    new CNode[K, V](bmp, tmparray, gen).toContracted(lev)
  }

  /** Returns a string representation of this CNode.
   *
   *  @param lev the current level in the trie
   *  @return a string representation
   */
  def string(lev: Int): String = s"CNode ${bitmap.toHexString}\n" + array.map(_.string(lev + 1)).mkString("\n")

  /** Returns a string representation of this CNode.
   */
  override def toString() = {
    def elems: Seq[String] = array.flatMap {
      case sn: SNode[K, V] @uc => Iterable.single(sn.kvPair._2.toString)
      case in: INode[K, V] @uc => Iterable.single(augmentString(in.toString).drop(14) + "(" + in.gen + ")")
      case basicNode           => throw new MatchError(basicNode)
    }
    s"CNode(sz: ${elems.size}; ${elems.sorted.mkString(", ")})"
  }
}

private[concurrent] object CNode {

  /** Creates a new node combining `x` and `y`, choosing an LNode if hash bits are exhausted (lev >= 35) to avoid further branching.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param x the first node
   *  @param xhc the hash code of the first node
   *  @param y the second node
   *  @param yhc the hash code of the second node
   *  @param lev the current level in the trie
   *  @param gen the generation to use for the new node
   *  @param equiv the equivalence relation for comparing keys
   *  @return the new node
   */
  def dual[K, V](x: SNode[K, V], xhc: Int, y: SNode[K, V], yhc: Int, lev: Int, gen: Gen, equiv: Equiv[K]): MainNode[K, V] = if (lev < 35) {
    val xidx = (xhc >>> lev) & 0x1f
    val yidx = (yhc >>> lev) & 0x1f
    val bmp = (1 << xidx) | (1 << yidx)
    if (xidx == yidx) {
      val subinode = new INode[K, V](gen, equiv)//(TrieMap.inodeupdater)
      subinode.mainnode = dual(x, xhc, y, yhc, lev + 5, gen, equiv)
      new CNode(bmp, Array(subinode), gen)
    } else {
      if (xidx < yidx) new CNode(bmp, Array(x, y), gen)
      else new CNode(bmp, Array(y, x), gen)
    }
  } else {
    new LNode(x.k, x.v, y.k, y.v, equiv)
  }

}


private[concurrent] case class RDCSS_Descriptor[K, V](old: INode[K, V], expectedmain: MainNode[K, V] | Null, nv: INode[K, V]) {
  /** Indicates whether this RDCSS descriptor has been committed.
   */
  @volatile var committed = false
}


/** A concurrent hash-trie or TrieMap is a concurrent thread-safe lock-free
 *  implementation of a hash array mapped trie. It is used to implement the
 *  concurrent map abstraction. It has particularly scalable concurrent insert
 *  and remove operations and is memory-efficient. It supports O(1), atomic,
 *  lock-free snapshots which are used to implement linearizable lock-free size,
 *  iterator and clear operations. The cost of evaluating the (lazy) snapshot is
 *  distributed across subsequent updates, thus making snapshot evaluation horizontally scalable.
 *
 *  For details, see: [[http://lampwww.epfl.ch/~prokopec/ctries-snapshot.pdf]]
 */
@SerialVersionUID(-5212455458703321708L)
final class TrieMap[K, V] private (r: AnyRef, rtupd: AtomicReferenceFieldUpdater[TrieMap[K, V], AnyRef] | Null, hashf: Hashing[K], ef: Equiv[K])
  extends scala.collection.mutable.AbstractMap[K, V]
    with scala.collection.concurrent.Map[K, V]
    with scala.collection.mutable.MapOps[K, V, TrieMap, TrieMap[K, V]]
    with scala.collection.MapFactoryDefaults[K, V, TrieMap, mutable.Iterable]
    with DefaultSerializable {

  private var hashingobj = if (hashf.isInstanceOf[Hashing.Default[?]]) new TrieMap.MangledHashing[K] else hashf
  private var equalityobj = ef
  @transient
  private var rootupdater: AtomicReferenceFieldUpdater[TrieMap[K, V], AnyRef] | Null = rtupd
  /** Returns the hashing function used to compute hash codes for keys in this TrieMap.
   */
  def hashing = hashingobj
  /** Returns the equivalence relation used by this TrieMap.
   */
  def equality = equalityobj
  @volatile private var root = r

  /** Initializes a new TrieMap with the specified hashing function and equivalence relation for key comparison.
   *
   *  @param hashf the hashing function
   *  @param ef the equivalence relation
   */
  def this(hashf: Hashing[K], ef: Equiv[K]) = this(
    INode.newRootNode(ef),
    AtomicReferenceFieldUpdater.newUpdater(classOf[TrieMap[K, V]], classOf[AnyRef], "root"),
    hashf,
    ef
  )

  /** Initializes a new TrieMap with default universal equivalence and hashing for key comparison.
   */
  def this() = this(Hashing.default, Equiv.universal)

  /** Returns the TrieMap companion object as the factory for creating new TrieMap instances.
   */
  override def mapFactory: MapFactory[TrieMap] = TrieMap

  /* internal methods */

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeObject(hashingobj)
    out.writeObject(equalityobj)

    val it = iterator
    while (it.hasNext) {
      val (k, v) = it.next()
      out.writeObject(k)
      out.writeObject(v)
    }
    out.writeObject(TrieMapSerializationEnd)
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    root = INode.newRootNode(equality)
    rootupdater = AtomicReferenceFieldUpdater.newUpdater(classOf[TrieMap[K, V]], classOf[AnyRef], "root")

    hashingobj = in.readObject().asInstanceOf[Hashing[K]]
    equalityobj = in.readObject().asInstanceOf[Equiv[K]]

    var obj: AnyRef = in.readObject()

    while (obj != TrieMapSerializationEnd) {
      obj = in.readObject()
      if (obj != TrieMapSerializationEnd) {
        val k = obj.asInstanceOf[K]
        val v = in.readObject().asInstanceOf[V]
        update(k, v)
      }
    }
  }

  private def CAS_ROOT(ov: AnyRef, nv: AnyRef) = rootupdater.nn.compareAndSet(this, ov, nv)

  private[collection] def readRoot(abort: Boolean = false): INode[K, V] = RDCSS_READ_ROOT(abort)

  private[concurrent] def RDCSS_READ_ROOT(abort: Boolean = false): INode[K, V] = {
    val r = /*READ*/root
    r match {
      case in: INode[K, V] @uc              => in
      case desc: RDCSS_Descriptor[K, V] @uc => RDCSS_Complete(abort)
      case x                                => throw new MatchError(x)
    }
  }

  @tailrec private def RDCSS_Complete(abort: Boolean): INode[K, V] = {
    val v = /*READ*/root
    v match {
      case in: INode[K, V] @uc => in
      case desc: RDCSS_Descriptor[K, V] @uc =>
        val RDCSS_Descriptor(ov, exp, nv) = desc
        if (abort) {
          if (CAS_ROOT(desc, ov)) ov
          else RDCSS_Complete(abort)
        } else {
          val oldmain = ov.gcasRead(this)
          if (oldmain eq exp) {
            if (CAS_ROOT(desc, nv)) {
              desc.committed = true
              nv
            } else RDCSS_Complete(abort)
          } else {
            if (CAS_ROOT(desc, ov)) ov
            else RDCSS_Complete(abort)
          }
        }
      case x => throw new MatchError(x)
    }
  }

  private def RDCSS_ROOT(ov: INode[K, V], expectedmain: MainNode[K, V] | Null, nv: INode[K, V]): Boolean = {
    val desc = RDCSS_Descriptor(ov, expectedmain, nv)
    if (CAS_ROOT(ov, desc)) {
      RDCSS_Complete(abort = false)
      /*READ*/desc.committed
    } else false
  }

  @tailrec private def inserthc(k: K, hc: Int, v: V): Unit = {
    val r = RDCSS_READ_ROOT()
    if (!r.rec_insert(k, v, hc, 0, null, r.gen, this)) inserthc(k, hc, v)
  }

  @tailrec private def insertifhc(k: K, hc: Int, v: V, cond: AnyRef, fullEquals: Boolean): Option[V] = {
    val r = RDCSS_READ_ROOT()

    val ret = r.rec_insertif(k, v, hc, cond, fullEquals, 0, null, r.gen, this)
    if (ret eq null) insertifhc(k, hc, v, cond, fullEquals)
    else ret
  }

  /** Finds the value associated with this key
   *
   *  @param k  the key to look up
   *  @param hc the hashcode of `k`
   *
   *  @return the value associated with `k` if it exists, or `INodeBase.NO_SUCH_ELEMENT_SENTINEL` if not found
   */
  @tailrec private def lookuphc(k: K, hc: Int): AnyRef = {
    val r = RDCSS_READ_ROOT()
    val res = r.rec_lookup(k, hc, 0, null, r.gen, this)
    if (res eq INodeBase.RESTART) lookuphc(k, hc)
    else res
  }

  /** Removes a key-value pair from the map
   *
   *  @param k the key to remove
   *  @param v the value to compare against the current value associated with the key
   *  @param removalPolicy policy deciding whether to remove `k` based on `v` and the
   *                       current value associated with `k` (Always, FullEquals, or ReferenceEq)
   *  @param hc the hashcode of `k`
   *  @return an `Option[V]` indicating the previous value
   */
  @tailrec private def removehc(k: K, v: V, removalPolicy: Int, hc: Int): Option[V] = {
    val r = RDCSS_READ_ROOT()
    val res = r.rec_remove(k, v, removalPolicy, hc, 0, null, r.gen, this)
    if (res ne null) res
    else removehc(k, v, removalPolicy, hc)
  }


  /** Returns a string representation of the trie structure rooted at this TrieMap’s root node.
   */
  def string: String = RDCSS_READ_ROOT().string(0)

  /* public methods */

  /** Checks if this TrieMap is a read-only snapshot, indicated by a null root updater.
   */
  def isReadOnly = rootupdater eq null

  /** Checks if this TrieMap is mutable, indicated by a non-null root updater.
   */
  def nonReadOnly = rootupdater ne null

  /** Returns a snapshot of this TrieMap.
   *  This operation is lock-free and linearizable.
   *
   *  The snapshot is lazily updated - the first time some branch
   *  in the snapshot or this TrieMap are accessed, they are rewritten.
   *  This means that the work of rebuilding both the snapshot and this
   *  TrieMap is distributed across all the threads doing updates or accesses
   *  subsequent to the snapshot creation.
   *
   *  @return a new mutable `TrieMap` snapshot that shares structure with this map
   */
  @tailrec def snapshot(): TrieMap[K, V] = {
    val r = RDCSS_READ_ROOT()
    val expmain = r.gcasRead(this)
    if (RDCSS_ROOT(r, expmain, r.copyToGen(new Gen, this))) new TrieMap(r.copyToGen(new Gen, this), rootupdater, hashing, equality)
    else snapshot()
  }

  /** Returns a read-only snapshot of this TrieMap.
   *  This operation is lock-free and linearizable.
   *
   *  The snapshot is lazily updated - the first time some branch
   *  of this TrieMap are accessed, it is rewritten. The work of creating
   *  the snapshot is thus distributed across subsequent updates
   *  and accesses on this TrieMap by all threads.
   *  Note that the snapshot itself is never rewritten unlike when calling
   *  the `snapshot` method, but the obtained snapshot cannot be modified.
   *
   *  This method is used by other methods such as `size` and `iterator`.
   *
   *  @return an immutable read-only snapshot of this map
   */
  @tailrec def readOnlySnapshot(): scala.collection.Map[K, V] = {
    val r = RDCSS_READ_ROOT()
    val expmain = r.gcasRead(this)
    if (RDCSS_ROOT(r, expmain, r.copyToGen(new Gen, this))) new TrieMap(r, null, hashing, equality)
    else readOnlySnapshot()
  }

  /** Atomically clears this TrieMap, removing all key-value pairs in a lock-free manner.
   */
  @tailrec override def clear(): Unit = {
    val r = RDCSS_READ_ROOT()
    if (!RDCSS_ROOT(r, r.gcasRead(this), INode.newRootNode[K, V](equality))) clear()
  }

  /** Computes the hash code for the specified key using this TrieMap’s hashing function.
   *
   *  @param k the key to compute the hash code for
   */
  def computeHash(k: K) = hashingobj.hash(k)

  @deprecated("Use getOrElse(k, null) instead.", "2.13.0")
  /** Deprecated. Looks up the value associated with the key, returning null if not found.
   *
   *  @param k the key to look up
   *  @return the value associated with the key, or null if not found
   */
  def lookup(k: K): V = {
    val hc = computeHash(k)
    val lookupRes = lookuphc(k, hc)
    val res = if (lookupRes == INodeBase.NO_SUCH_ELEMENT_SENTINEL) null else lookupRes
    res.asInstanceOf[V]
  }

  /** Returns the value associated with the key, throwing NoSuchElementException if not found.
   *
   *  @param k the key to look up
   *  @return the value associated with the key
   *  @throws NoSuchElementException if the key is not found
   */
  override def apply(k: K): V = {
    val hc = computeHash(k)
    val res = lookuphc(k, hc)
    if (res eq INodeBase.NO_SUCH_ELEMENT_SENTINEL) throw new NoSuchElementException
    else res.asInstanceOf[V]
  }

  /** Returns the value associated with the specified key.
   *
   *  @param k the key to look up
   *  @return the value associated with the key, or None if not found
   */
  def get(k: K): Option[V] = {
    val hc = computeHash(k)
    val res = lookuphc(k, hc)
    if (res eq INodeBase.NO_SUCH_ELEMENT_SENTINEL) None else Some(res).asInstanceOf[Option[V]]
  }

  /** Inserts or overwrites the key-value pair, returning the previous value if any.
   *
   *  @param key the key to insert
   *  @param value the value to associate with the key
   *  @return the previous value associated with the key, or None if not found
   */
  override def put(key: K, value: V): Option[V] = {
    val hc = computeHash(key)
    insertifhc(key, hc, value, INode.KEY_PRESENT_OR_ABSENT, fullEquals = false /* unused */)
  }

  /** Inserts or updates the value associated with the specified key.
   *
   *  @param k the key to update
   *  @param v the new value to associate with the key
   */
  override def update(k: K, v: V): Unit = {
    val hc = computeHash(k)
    inserthc(k, hc, v)
  }

  /** Adds a key-value pair to this TrieMap by delegating to `update`, returning the TrieMap for method chaining.
   *
   *  @param kv the key-value pair to add
   */
  def addOne(kv: (K, V)) = {
    update(kv._1, kv._2)
    this
  }

  /** Removes the key-value pair with the specified key unconditionally, returning the previous value if any.
   *
   *  @param k the key to remove
   *  @return the previous value associated with the key, or None if not found
   */
  override def remove(k: K): Option[V] = {
    val hc = computeHash(k)
    removehc(k = k, v = null.asInstanceOf[V], RemovalPolicy.Always, hc = hc)
  }

  /** Removes the key-value pair with the specified key by delegating to `remove`, returning the TrieMap for method chaining.
   *
   *  @param k the key to remove
   */
  def subtractOne(k: K) = {
    remove(k)
    this
  }

  /** Inserts the key-value pair only if the key is not already present, returning the previous value if any.
   *
   *  @param k the key to insert
   *  @param v the value to associate with the key
   *  @return the previous value associated with the key, or None if not found
   */
  def putIfAbsent(k: K, v: V): Option[V] = {
    val hc = computeHash(k)
    insertifhc(k, hc, v, INode.KEY_ABSENT, fullEquals = false /* unused */)
  }

  // TODO once computeIfAbsent is added to concurrent.Map,
  // move the comment there and tweak the 'at most once' part
  /** Returns the value associated with the key, or computes and inserts it using `defaultValue` if absent. The `defaultValue` is evaluated lazily and at most once.
   *
   *  If the specified mapping function throws an exception,
   *  that exception is rethrown.
   *
   *  Note: This method will invoke `defaultValue` at most once.
   *  However, `defaultValue` may be invoked without the result being added to the map if
   *  a concurrent process is also trying to add a value corresponding to the
   *  same key `k`.
   *
   *  @param k      the key to modify
   *  @param defaultValue     the expression that computes the value
   *  @return       the newly added value
   */
  override def getOrElseUpdate(k: K, @deprecatedName("op", since="2.13.13") defaultValue: => V): V = {
    val hc = computeHash(k)
    lookuphc(k, hc) match {
      case INodeBase.NO_SUCH_ELEMENT_SENTINEL =>
        val v = defaultValue
        insertifhc(k, hc, v, INode.KEY_ABSENT, fullEquals = false /* unused */) match {
          case Some(oldValue) => oldValue
          case None => v
        }
      case oldValue => oldValue.asInstanceOf[V]
    }
  }

  /** Removes the key-value pair only if the current value matches `v`, returning true if removed.
   *
   *  @param k the key to remove
   *  @param v the value to compare against the current value associated with the key
   *  @return true if the key-value pair was removed, false otherwise
   */
  def remove(k: K, v: V): Boolean = {
    val hc = computeHash(k)
    removehc(k, v, RemovalPolicy.FullEquals, hc).nonEmpty
  }

  override private[collection] def removeRefEq(k: K, v: V): Boolean = {
    val hc = computeHash(k)
    removehc(k, v, RemovalPolicy.ReferenceEq, hc).nonEmpty
  }

  /** Replaces the value associated with the key only if the current value matches `oldvalue`, returning true if replaced.
   *
   *  @param k the key to replace
   *  @param oldvalue the expected old value
   *  @param newvalue the new value to associate with the key
   *  @return true if the replacement was successful, false otherwise
   */
  def replace(k: K, oldvalue: V, newvalue: V): Boolean = {
    val hc = computeHash(k)
    insertifhc(k, hc, newvalue, oldvalue.asInstanceOf[AnyRef], fullEquals = true).nonEmpty
  }

  override private[collection] def replaceRefEq(k: K, oldValue: V, newValue: V): Boolean = {
    val hc = computeHash(k)
    insertifhc(k, hc, newValue, oldValue.asInstanceOf[AnyRef], fullEquals = false).nonEmpty
  }

  /** Replaces the value associated with the key only if the key is present, returning the previous value if any.
   *
   *  @param k the key to replace
   *  @param v the new value to associate with the key
   *  @return the previous value associated with the key, or None if not found
   */
  def replace(k: K, v: V): Option[V] = {
    val hc = computeHash(k)
    insertifhc(k, hc, v, INode.KEY_PRESENT, fullEquals = false /* unused */)
  }

  /** Returns an iterator over the key-value pairs, using a read-only snapshot if the TrieMap is mutable.
   */
  def iterator: Iterator[(K, V)] = {
    if (nonReadOnly) readOnlySnapshot().iterator
    else new TrieMapIterator(0, this)
  }

  ////////////////////////////////////////////////////////////////////////////
  //
  // scala/bug#10177 These methods need overrides as the inherited implementations
  // call `.iterator` more than once, which doesn't guarantee a coherent
  // view of the data if there is a concurrent writer
  // Note that the we don't need overrides for keysIterator or valuesIterator
  // TrieMapTest validates the behaviour.
  /** Returns an iterable over the values, using a read-only snapshot if the TrieMap is mutable.
   */
  override def values: Iterable[V] = {
    if (nonReadOnly) readOnlySnapshot().values
    else super.values
  }
  /** Returns a set of the keys, using a read-only snapshot if the TrieMap is mutable.
   */
  override def keySet: Set[K] = {
    if (nonReadOnly) readOnlySnapshot().keySet
    else super.keySet
  }

  /** Returns a view of this TrieMap, using a read-only snapshot if the TrieMap is mutable.
   */
  override def view: MapView[K, V] = if (nonReadOnly) readOnlySnapshot().view else super.view

  @deprecated("Use .view.filterKeys(f). A future version will include a strict version of this method (for now, .view.filterKeys(p).toMap).", "2.13.0")
  /** Returns a view of this TrieMap with keys filtered by the specified predicate.
   *
   *  @param p the predicate to filter keys by
   */
  override def filterKeys(p: K => Boolean): collection.MapView[K, V]^{p} = view.filterKeys(p)

  @deprecated("Use .view.mapValues(f). A future version will include a strict version of this method (for now, .view.mapValues(f).toMap).", "2.13.0")
  /** Returns a view of this TrieMap with values transformed by the specified function.
   *
   *  @tparam W the type of the transformed values
   *  @param f the function to transform values with
   */
  override def mapValues[W](f: V => W): collection.MapView[K, W]^{f} = view.mapValues(f)
  // END extra overrides
  ///////////////////////////////////////////////////////////////////

  /** Returns the size of this TrieMap, using a read-only snapshot if the TrieMap is mutable.
   */
  override def size: Int =
    if (nonReadOnly) readOnlySnapshot().size
    else RDCSS_READ_ROOT().cachedSize(this)
  /** Returns the known size of this TrieMap, or -1 if the size is not precomputed due to concurrent modifications.
   */
  override def knownSize: Int =
    if (nonReadOnly) -1
    else RDCSS_READ_ROOT().knownSize(this)
  /** Checks if this TrieMap is empty, using a read-only snapshot if the TrieMap is mutable.
   */
  override def isEmpty: Boolean =
    (if (nonReadOnly) readOnlySnapshot() else this).sizeIs == 0 // sizeIs checks knownSize
  /** Returns the class name of this TrieMap for use in string representation.
   */
  override protected def className = "TrieMap"

  /** Returns the last key-value pair in this TrieMap, or `None` if this TrieMap is empty or if the last element cannot be retrieved.
   */
  override def lastOption: Option[(K, V)] = if (isEmpty) None else Try(last).toOption
}


@SerialVersionUID(3L)
object TrieMap extends MapFactory[TrieMap] {

  /** Returns a new empty TrieMap with default hashing and equivalence.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @return an empty TrieMap
   */
  def empty[K, V]: TrieMap[K, V] = new TrieMap[K, V]

  /** Creates a new TrieMap populated with the key-value pairs from the specified iterable.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param it the iterable to create the TrieMap from
   *  @return a new TrieMap containing the key-value pairs from the iterable
   */
  def from[K, V](it: IterableOnce[(K, V)]^): TrieMap[K, V] = new TrieMap[K, V]() ++= it

  /** Returns a new builder for constructing TrieMaps from key-value pairs.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @return a new builder for TrieMap
   */
  def newBuilder[K, V]: mutable.GrowableBuilder[(K, V), TrieMap[K, V]] = new GrowableBuilder(empty[K, V])

  /** The atomic reference field updater for INodeBase.
   */
  @transient
  val inodeupdater: AtomicReferenceFieldUpdater[INodeBase[?, ?], MainNode[?, ?]] = AtomicReferenceFieldUpdater.newUpdater(classOf[INodeBase[?, ?]], classOf[MainNode[?, ?]], "mainnode")

  /** A hashing function that mangles the hash code of keys.
   *
   *  @tparam K the type of keys
   */
  class MangledHashing[K] extends Hashing[K] {
    /** Computes and mangles the hash code of the key for better distribution in the TrieMap.
     *
     *  @param k the key to compute the hash code for
     *  @return the hash code
     */
    def hash(k: K): Int = scala.util.hashing.byteswap32(k.##)
  }

  private[concurrent] object RemovalPolicy {
    /** A removal policy that always removes the key.
     */
    final val Always = 0
    /** A removal policy that removes the key if the values are equal.
     */
    final val FullEquals = 1
    /** A removal policy that removes the key if the values are reference-equal.
     */
    final val ReferenceEq = 2

    /** Checks if the value should be removed based on the removal policy.
     *
     *  @tparam V the type of values
     *  @param removalPolicy the removal policy to use
     *  @param a one value to compare
     *  @param b the other value to compare
     *  @return true if the value should be removed, false otherwise
     */
    def shouldRemove[V](removalPolicy: Int)(a: V, b: V): Boolean =
      removalPolicy match {
        case Always      => true
        case FullEquals  => a == b
        case ReferenceEq => a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]
      }
  }
}

// non-final as an extension point for parallel collections
private[collection] class TrieMapIterator[K, V](var level: Int, private var ct: TrieMap[K, V], mustInit: Boolean = true) extends AbstractIterator[(K, V)] {
  self: TrieMapIterator[K, V] =>
  private val stack = new Array[Array[BasicNode]](7)
  private val stackpos = new Array[Int](7)
  private var depth = -1
  @annotation.stableNull private var subiter: Iterator[(K, V)] | Null = null
  @annotation.stableNull private var current: KVNode[K, V] | Null = null

  if (mustInit) initialize()

  /** Checks if there are more elements in this iterator.
   */
  def hasNext = (current ne null) || (subiter ne null)

  /** Returns the next element in this iterator.
   */
  def next(): (K, V) = {
    if (subiter ne null) {
      val r = subiter.next()
      checkSubiter()
      r
    } else if (current ne null) {
      val r = current.kvPair
      advance()
      r
    } else Iterator.empty.next()
  }

  private def readin(in: INode[K, V]) = in.gcasRead(ct) match {
    case cn: CNode[K, V] =>
      depth += 1
      stack(depth) = cn.array
      stackpos(depth) = -1
      advance()
    case tn: TNode[K, V] =>
      current = tn
    case ln: LNode[K, V] =>
      subiter = ln.entries.iterator
      checkSubiter()
    case null =>
      current = null
    case mainNode => throw new MatchError(mainNode)
  }

  private def checkSubiter() = if (!subiter.nn.hasNext) {
    subiter = null
    advance()
  }

  private def initialize(): Unit = {
    assert(ct.isReadOnly)

    val r = ct.RDCSS_READ_ROOT()
    readin(r)
  }

  /** Advances this iterator to the next element.
   */
  @tailrec
  final def advance(): Unit = if (depth >= 0) {
    val npos = stackpos(depth) + 1
    if (npos < stack(depth).length) {
      stackpos(depth) = npos
      stack(depth)(npos) match {
        case sn: SNode[K, V] @uc => current = sn
        case in: INode[K, V] @uc => readin(in)
        case basicNode           => throw new MatchError(basicNode)
      }
    } else {
      depth -= 1
      advance()
    }
  } else current = null

  /** Creates a new iterator with the specified parameters.
   *
   *  @param _lev the current level in the trie
   *  @param _ct the TrieMap instance
   *  @param _mustInit whether the iterator must be initialized
   *  @return a new iterator
   */
  protected def newIterator(_lev: Int, _ct: TrieMap[K, V], _mustInit: Boolean): TrieMapIterator[K, V] = new TrieMapIterator[K, V](_lev, _ct, _mustInit)

  /** Duplicates this iterator to the specified iterator.
   *
   *  @param it the iterator to duplicate to
   */
  protected def dupTo(it: TrieMapIterator[K, V]): Unit = {
    it.level = this.level
    it.ct = this.ct
    it.depth = this.depth
    it.current = this.current

    // these need a deep copy
    Array.copy(this.stack, 0, it.stack, 0, 7)
    Array.copy(this.stackpos, 0, it.stackpos, 0, 7)

    // this one needs to be evaluated
    if (this.subiter == null) it.subiter = null
    else {
      val lst = this.subiter.to(immutable.List)
      this.subiter = lst.iterator
      it.subiter = lst.iterator
    }
  }

  /** Returns a sequence of iterators over subsets of this iterator.
   *  It's used to ease the implementation of splitters for a parallel version of the TrieMap.
   */
  protected def subdivide(): Seq[Iterator[(K, V)]] = if (subiter ne null) {
    // the case where an LNode is being iterated
    val it = newIterator(level + 1, ct, _mustInit = false)
    it.depth = -1
    it.subiter = this.subiter
    it.current = null
    this.subiter = null
    advance()
    this.level += 1
    Seq(it, this)
  } else if (depth == -1) {
    this.level += 1
    Seq(this)
  } else {
    var d = 0
    while (d <= depth) {
      val rem = stack(d).length - 1 - stackpos(d)
      if (rem > 0) {
        val (arr1, arr2) = stack(d).drop(stackpos(d) + 1).splitAt(rem / 2)
        stack(d) = arr1
        stackpos(d) = -1
        val it = newIterator(level + 1, ct, _mustInit = false)
        it.stack(0) = arr2
        it.stackpos(0) = -1
        it.depth = 0
        it.advance() // <-- fix it
        this.level += 1
        return Seq(this, it)
      }
      d += 1
    }
    this.level += 1
    Seq(this)
  }

}

/** Only used for ctrie serialization. */
@SerialVersionUID(3L)
private[concurrent] case object TrieMapSerializationEnd
