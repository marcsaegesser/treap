package treaps

import org.scalacheck._
import org.scalacheck.Prop.forAll

object TreapProperties extends Properties("Treap") {
  import Gen._
  import Arbitrary.arbitrary
  import DefaultPriorityGenerator._

  type KVP = (Int, String)
  type KVPP = (Int, String, Int)

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

  import Treap._

  def validTreap[K,V](t: Treap[K,V])(implicit o: Ordering[K]): Boolean =
    t match {
      case LeafNode(_, _, _) => true
      case LeftNode(k, v, p, l) => o.gteq(k, l.k) && p >= l.p
      case RightNode(k, v, p, r) => o.lteq(k, r.k) && p >= r.p
      case FullNode(k, v, p, l, r) => o.lteq(k, r.k) && o.gteq(k, l.k) && p >= r.p && p >= l.p
      case EmptyTreap => throw new Exception("Treap contains EmptyTreap")
    }

  property("toList is sorted") = forAll { l: List[KVP] =>
    val t = Treap(l:_*)
    val sorted = deDup(l).sortBy(_._1)
    if(t.toList != sorted) println("" + t)
    t.toList == sorted
  }

  property("Treaps are valid") = forAll { l: List[KVP] =>
    val t = Treap(l:_*)
    t.foldLeft(true)((a, t) => a && validTreap(t))
  }

  property("find") = forAll { l: List[KVP] =>
    val t = Treap(l:_*)
    val dl = deDup(l)
    dl.forall{ case ((k, v)) => t.find(k) == Option(v) }
  }

  property("find non-existent keys") = forAll (listOf(genPosKeys)) { l =>
    val t = Treap(l:_*)
    deDup(l).forall{ case ((k, v)) => t.find(-k) == None }
  }

  property("foldLeft") = forAll { l: List[KVP] =>
    val t = Treap(l:_*)
    val sum = t.foldLeft(0)((a, t) => a + t.k)
    sum == deDup(l).map(_._1).sum
  }

  property("foldLeft is in-order") = forAll { l: List[KVP] =>
    import scala.collection.mutable.StringBuilder
    val t = Treap(l:_*)
    t.foldLeft(StringBuilder.newBuilder)((a, t) => a.append(t.v)) ==
    deDup(l).sortBy(_._1).foldLeft(StringBuilder.newBuilder)((a, t) => a.append(t._2))
  }

  property("remove") = forAll { l: List[KVP] =>
    val t = Treap(l:_*)
    val result = l.foldLeft(t)((a, k) => a.remove(k._1))
    result.isEmpty
  }

  property("size") = forAll { l: List[KVP] =>
    val t = Treap(l:_*)
    deDup(l).size == t.size
  }

  property("fold") = forAll { l: List[KVP] =>
    def helper[K,V](l: Treap[K,V], k: K, v: V, r: Treap[K,V]): List[K] =
      l.fold(List.empty[K])(helper) ++ List(k) ++ r.fold(List.empty[K])(helper)
    val t = Treap(l:_*)
    val result = t.fold(List.empty[Int])(helper)
    val sorted = deDup(l).sortBy(_._1)
    sorted == result
  }
}
