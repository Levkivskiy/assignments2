package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(genHeap, const(empty))
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2, insert any two elements into an empty heap") = forAll { (fir: Int, sec: Int) =>
    val head = insert(fir, insert(sec, empty))
    findMin(head) == math.min(fir, sec)
  }

  property("min1") = forAll { v: Int =>
    val heap = insert(v, empty)
    findMin(heap) == v
  }

    property("gen3, delete min of heap with heap is empty") = forAll { a: Int =>
    val head = insert(a, empty)
    val afterDel = deleteMin(head)
    isEmpty(afterDel)
  }

  property("gen4, heaps for sorting") = forAll { head: H =>
    def isSorted(heap: H): Boolean = {
      def sortFrom(sorted: List[A], heap: H): List[A] = {
        val currtMin = findMin(heap)
        val smallerHeap = deleteMin(heap)
        if (isEmpty(smallerHeap))
          currtMin :: sorted
        else
          sortFrom(currtMin :: sorted, smallerHeap)
      }
      val xs = sortFrom(Nil, heap)
      (xs, xs.tail).zipped.forall(_ >= _)
    }

    isSorted(head)
  }

  property("gen5, min(x meld y)") = forAll { (xsFir: H, xsSec: H) =>
    val melded = meld(xsFir, xsSec)
    findMin(melded) == math.min(findMin(xsFir), findMin(xsSec))
  }

  property("gen, melding heaps") = forAll { (h:H, i:H) =>
    val minH = findMin(h)
    val minI = findMin(i)
    val mergHI = meld(h, i)
    findMin(mergHI) == (if (minH < minI) minH else minI)
  }

  property("gen, associative meld") = forAll { (h:H, i:H, j:H) =>
    val a = meld(meld(h, i), j)
    val b = meld(h, meld(i, j))
    a == b
  }
}