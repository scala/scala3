package dotty.tools.dotc.core

import dotty.tools.dotc.core.Decorators.*

import org.junit.Test

class DecoratorsTest:

  private case class Box(i: Int)

  private val lengths = Vector(0, 1, 2, 31, 32, 33)

  private def nestedBoxes(outer: Int, inner: Int): Vector[Vector[Box]] =
    Vector.tabulate(outer)(i => Vector.tabulate(inner)(j => Box(i * 1000 + j)))

  @Test def vectorMapConserve: Unit =
    for len <- lengths do
      val xs = Vector.tabulate(len)(Box(_))
      var calls = 0
      val same = xs.mapConserve { x =>
        calls += 1
        x
      }
      assert(same eq xs)
      assert(calls == len)

      if len > 0 then
        val changedAt = len / 2
        calls = 0
        val changed = xs.mapConserve { x =>
          calls += 1
          if x.i == changedAt then Box(-x.i - 1) else x
        }
        assert(!(changed eq xs))
        assert(changed == xs.updated(changedAt, Box(-changedAt - 1)))
        assert(calls == len)

        for changedAt <- Vector(0, len - 1).distinct do
          calls = 0
          val changed = xs.mapConserve { x =>
            calls += 1
            if x.i == changedAt then Box(-x.i - 1) else x
          }
          assert(!(changed eq xs))
          assert(changed == xs.updated(changedAt, Box(-changedAt - 1)))
          assert(calls == len)

        calls = 0
        val allChanged = xs.mapConserve { x =>
          calls += 1
          Box(-x.i - 1)
        }
        assert(!(allChanged eq xs))
        assert(allChanged == xs.map(x => Box(-x.i - 1)))
        assert(calls == len)

  @Test def vectorZipFoldLeft: Unit =
    for len <- lengths do
      val xs = Vector.tabulate(len)(Box(_))
      val ys = Vector.tabulate(len)(i => Box(i * 10))
      val folded = xs.zipFoldLeft(ys)(0)((acc, x, y) => acc + x.i + y.i)
      assert(folded == xs.zip(ys).foldLeft(0)((acc, xy) => acc + xy._1.i + xy._2.i))

      if len > 0 then
        val shorter = xs.zipFoldLeft(ys.dropRight(1))(Vector.empty[Int])((acc, x, y) => acc :+ (x.i + y.i))
        assert(shorter.length == len - 1)
        assert(shorter == xs.lazyZip(ys.dropRight(1)).map(_.i + _.i))

  @Test def vectorFilterConserve: Unit =
    for len <- lengths do
      val xs = Vector.tabulate(len)(Box(_))
      var calls = 0
      val same = xs.filterConserve { _ =>
        calls += 1
        true
      }
      assert(same eq xs)
      assert(calls == len)

      calls = 0
      val empty = xs.filterConserve { _ =>
        calls += 1
        false
      }
      assert(empty.isEmpty)
      assert(calls == len)

      if len > 0 then
        calls = 0
        val filtered = xs.filterConserve { x =>
          calls += 1
          x.i % 3 != 1
        }
        assert(filtered == xs.filter(_.i % 3 != 1))
        assert(calls == len)

        calls = 0
        val dropFirst = xs.filterConserve { x =>
          calls += 1
          x.i != 0
        }
        assert(dropFirst == xs.drop(1))
        assert(calls == len)

        calls = 0
        val dropLast = xs.filterConserve { x =>
          calls += 1
          x.i != len - 1
        }
        assert(dropLast == xs.dropRight(1))
        assert(calls == len)

  @Test def vectorZipWithConserve: Unit =
    for len <- lengths do
      val xs = Vector.tabulate(len)(Box(_))
      val ys = Vector.tabulate(len)(identity)
      var calls = 0
      val same = xs.zipWithConserve(ys) { (x, _) =>
        calls += 1
        x
      }
      assert(same eq xs)
      assert(calls == len)

      if len > 0 then
        val changedAt = len / 2
        calls = 0
        val changed = xs.zipWithConserve(ys) { (x, y) =>
          calls += 1
          if y == changedAt then Box(-x.i - 1) else x
        }
        assert(!(changed eq xs))
        assert(changed == xs.updated(changedAt, Box(-changedAt - 1)))
        assert(calls == len)

        for changedAt <- Vector(0, len - 1).distinct do
          calls = 0
          val changed = xs.zipWithConserve(ys) { (x, y) =>
            calls += 1
            if y == changedAt then Box(-x.i - 1) else x
          }
          assert(!(changed eq xs))
          assert(changed == xs.updated(changedAt, Box(-changedAt - 1)))
          assert(calls == len)

        calls = 0
        val shorter = xs.zipWithConserve(ys.dropRight(1)) { (x, _) =>
          calls += 1
          x
        }
        assert(shorter == xs.dropRight(1))
        assert(calls == len - 1)

  @Test def vectorZipMap: Unit =
    for len <- lengths do
      val xs = Vector.tabulate(len)(Box(_))
      val ys = Vector.tabulate(len)(identity)
      var calls = 0
      val mapped = xs.zipMap(ys) { (x, y) =>
        calls += 1
        Box(x.i + y)
      }
      assert(mapped == xs.lazyZip(ys).map((x, y) => Box(x.i + y)))
      assert(calls == len)

      calls = 0
      val shorter = xs.zipMap(ys.dropRight(1)) { (x, y) =>
        calls += 1
        Box(x.i + y)
      }
      assert(shorter == xs.dropRight(1).lazyZip(ys.dropRight(1)).map((x, y) => Box(x.i + y)))
      assert(calls == math.max(0, len - 1))

  @Test def vectorZipMap3: Unit =
    for len <- lengths do
      val xs = Vector.tabulate(len)(Box(_))
      val ys = Vector.tabulate(len)(identity)
      val zs = Vector.tabulate(len)(_ * 2)
      var calls = 0
      val mapped = xs.zipMap(ys, zs) { (x, y, z) =>
        calls += 1
        Box(x.i + y + z)
      }
      assert(mapped == xs.lazyZip(ys).lazyZip(zs).map((x, y, z) => Box(x.i + y + z)))
      assert(calls == len)

      calls = 0
      val shorter = xs.zipMap(ys.dropRight(1), zs.dropRight(2)) { (x, y, z) =>
        calls += 1
        Box(x.i + y + z)
      }
      assert(shorter == xs.dropRight(2).lazyZip(ys.dropRight(1)).lazyZip(zs.dropRight(2)).map((x, y, z) => Box(x.i + y + z)))
      assert(calls == math.max(0, len - 2))

  @Test def vectorMapWithIndexConserve: Unit =
    for len <- lengths do
      val xs = Vector.tabulate(len)(Box(_))
      var calls = 0
      val same = xs.mapWithIndexConserve { (x, _) =>
        calls += 1
        x
      }
      assert(same eq xs)
      assert(calls == len)

      if len > 0 then
        val changedAt = len / 2
        calls = 0
        val changed = xs.mapWithIndexConserve { (x, i) =>
          calls += 1
          if i == changedAt then Box(-x.i - 1) else x
        }
        assert(!(changed eq xs))
        assert(changed == xs.updated(changedAt, Box(-changedAt - 1)))
        assert(calls == len)

        for changedAt <- Vector(0, len - 1).distinct do
          calls = 0
          val changed = xs.mapWithIndexConserve { (x, i) =>
            calls += 1
            if i == changedAt then Box(-x.i - 1) else x
          }
          assert(!(changed eq xs))
          assert(changed == xs.updated(changedAt, Box(-changedAt - 1)))
          assert(calls == len)

  @Test def vectorNestedMap: Unit =
    for
      outer <- lengths
      inner <- Vector(0, 1, 2, 31, 32, 33)
    do
      val xss = nestedBoxes(outer, inner)
      var calls = 0
      val mapped = xss.nestedMap { x =>
        calls += 1
        Box(x.i + 1)
      }
      assert(mapped == xss.map(_.map(x => Box(x.i + 1))))
      assert(calls == outer * inner)

  @Test def vectorNestedMapConserve: Unit =
    for
      outer <- lengths
      inner <- Vector(0, 1, 2, 31, 32, 33)
    do
      val xss = nestedBoxes(outer, inner)
      var calls = 0
      val same = xss.nestedMapConserve { x =>
        calls += 1
        x
      }
      assert(same eq xss)
      assert(calls == outer * inner)

      if outer > 0 && inner > 0 then
        val changedOuter = outer / 2
        val changedInner = inner / 2
        val target = changedOuter * 1000 + changedInner
        calls = 0
        val changed = xss.nestedMapConserve { x =>
          calls += 1
          if x.i == target then Box(-x.i - 1) else x
        }
        assert(!(changed eq xss))
        assert(changed == xss.updated(changedOuter, xss(changedOuter).updated(changedInner, Box(-target - 1))))
        assert(calls == outer * inner)

  @Test def vectorNestedSearch: Unit =
    for
      outer <- lengths
      inner <- Vector(0, 1, 2, 31, 32, 33)
    do
      val xss = nestedBoxes(outer, inner)
      val total = outer * inner
      var calls = 0
      assert(!xss.nestedExists { x =>
        calls += 1
        x.i == -1
      })
      assert(calls == total)

      calls = 0
      assert(xss.nestedFind { x =>
        calls += 1
        x.i == -1
      }.isEmpty)
      assert(calls == total)

      if total > 0 then
        val targetIndex = total / 2
        val target = targetIndex / inner * 1000 + targetIndex % inner
        calls = 0
        assert(xss.nestedExists { x =>
          calls += 1
          x.i == target
        })
        assert(calls == targetIndex + 1)

        calls = 0
        assert(xss.nestedFind { x =>
          calls += 1
          x.i == target
        }.contains(Box(target)))
        assert(calls == targetIndex + 1)
