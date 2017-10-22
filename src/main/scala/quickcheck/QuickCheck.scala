package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("sorted heap") = forAll { (g: H) =>

    def res_rec(r: List[Int], h: H): List[Int] = {
      if (isEmpty(h)) r
      else {
        val min = findMin(h) :: r
        res_rec(min, deleteMin(h))
      }
    }
    val res = res_rec(List(), g)
    res == res.sorted.reverse

  }
  

  property("meld") = forAll { (h: H, i: H) =>
    findMin(meld(h, i)) == { if (findMin(h) < findMin(i)) findMin(h) else findMin(i) }
    //    findMin(meld(h, i)) == findMin(h) || findMin(meld(h, i)) == findMin(i) 
  }
  property("min of 2") = forAll { (i: Int, j: Int) =>
    findMin(insert(j, insert(i, empty))) == { if (i < j) i else j }
  }
  property("insert and delete from empty") = forAll { (i: Int) =>
    isEmpty(deleteMin(insert(i, empty)))
  }
  property("find bug in deleteMin") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    deleteMin(insert(m, h)) == h

  }
  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def remMin(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: remMin(deleteMin(ts), as)
    }
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = remMin(meld1, Nil)
    val xs2 = remMin(meld2, Nil)
    xs1 == xs2
  }
}
