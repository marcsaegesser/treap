package treaps

import scala.util.Sorting
import org.scalacheck._
import org.scalacheck.Prop.forAll

object BSTProperties extends Properties("Binary Search Tree") {
  import Gen._
  import Arbitrary.arbitrary

  type KVP = (Int, String)

  val genKVP =
    for {
      k <- arbitrary[Int]
      v <- arbitrary[String]
    } yield (k, v)

  lazy val arbKVP = Arbitrary(genKVP)

  val genPosKeys =
    for {
      k <- posNum[Int]
      v <- arbitrary[String]
    } yield (k, v)

  object KVPOrdering extends Ordering[KVP] {
    def compare(a: KVP, b: KVP) = a._1 compare b._1
  }

  def deDup(l: List[KVP]): List[KVP] =
    l.groupBy(_._1)
      .mapValues( l => l.last)
      .values.toList


  property("toList is sorted") = forAll { l: List[KVP] =>
    val t = Treap(l:_*)
    val sorted = deDup(l).sortBy(_._1)
    t.toList == sorted
  }

  property("find") = forAll { l: List[KVP] =>
    val t = Treap(l:_*)
    val dl = deDup(l)
    dl.forall{ case ((k, v)) => t.find(k) == Option(v) }
  }

  property("find non-existent keys") = forAll (listOf(genPosKeys)) { l =>
    val t = Treap(l:_*)
    val dl = deDup(l)
    dl.forall{ case ((k, v)) => t.find(-k) == None }
  }
}
