package persistence

import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Prop}
import scala.math.Ordering

class SplayTreeTest extends ScalaCheckSuite:

  given [K: Arbitrary: Ordering, V: Arbitrary]: Arbitrary[SplayTree[K, V]] =
    Arbitrary(Arbitrary.arbitrary[List[(K, V)]].map(SplayTree.fromIterable))

  property("constructed in order"):
    Prop.forAll((xs: List[Int]) =>
      val t = xs.foldLeft(SplayTree.empty[Int, Int])((acc, x) => acc.put(x, x))
      assertEquals(t.size, xs.size)
      assertEquals(xs.iterator.foldLeft(true)((acc, x) => acc && t.contains(x)), true)
      assertEquals(xs.iterator.toList, xs)
      assertEquals(t.entry, if xs.nonEmpty then Some(xs.last -> xs.last) else None)
    )

  property("accessing element splays to root"):
    Prop.forAll((t: SplayTree[Int, Int], n0: Int) =>
      if t.size > 0 then
        val n = (n0 % t.size).abs
        val (k, v) = t.iterator.drop(n).toList.head
        if t.iterator.filter(kv => kv(0) == k).size == 1 then
          val result = t.get(k).get
          assertEquals(result(1), v)
          val t2 = result(0)
          assertEquals(t2.entry, Some(k -> v))
    )

  property("elements inserted at root"):
    Prop.forAll((t: SplayTree[Int, Int], x: Int) =>
      val t2 = t.put(x, x)
      assertEquals(t2.entry, Some(x -> x))
    )

  property("remove"):
    Prop.forAll((t: SplayTree[Int, Int], n0: Int) =>
      if t.size > 0 then
        val n = (n0 % t.size).abs
        val (k, v) = t.iterator.drop(n).toList.head
        val t2 = t.remove(k)
        assertEquals(t2.size, t.size - 1)
    )

  property("splitAt"):
    Prop.forAll((t: SplayTree[Int, Int], k: Int) =>
      val (pfx, sfx) = t.splitAt(k)
      assertEquals(pfx.size + sfx.size, t.size)
      val entries = t.iterator.toList
      val (lpfx, lsfx) = entries.splitAt(entries.segmentLength(_(0) <= k))
      assertEquals(pfx.iterator.toList.sorted, lpfx.sorted)
      assertEquals(sfx.iterator.toList.sorted, lsfx.sorted)
    )
