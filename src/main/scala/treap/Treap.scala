package treaps

sealed trait Treap[+K,+V] {
  import Treap._

  def add[KK >: K,VV >: V](k: KK, v: VV)(implicit o: Ordering[KK], p: PriorityGenerator): Treap[KK,VV]
  def remove[KK >: K](x: KK)(implicit o: Ordering[KK]): Treap[KK,V]
  def find[KK >: K](x: KK)(implicit o: Ordering[KK]): Option[V]

  def toList: List[(K, V)]
}

object Treap {
  type Priority = Int

  trait PriorityGenerator { def apply(): Priority }

  def apply[K,V](kvps: (K,V)*)(implicit o: Ordering[K]): Treap[K,V] = kvps.toList.foldLeft(Treap.empty[K,V])((a, kvp) => a.add(kvp._1, kvp._2))

  def empty[K,V]: Treap[K,V] = EmptyTreap

  case object EmptyTreap extends Treap[Nothing,Nothing] {
    def add[K,V](k: K, v: V)(implicit o: Ordering[K], pg: PriorityGenerator): Treap[K,V] =
      Leaf[K,V](k, v, pg())

    def remove[KK >: Nothing](x: KK)(implicit o: Ordering[KK]): Treap[KK,Nothing] = throw new NoSuchElementException("remove on empty Treap")

    def find[KK >: Nothing](x: KK)(implicit o: Ordering[KK]): Option[Nothing] = None

    def toList = List()
  }

  import CompareHelper._

  case class Leaf[K,V](k: K, v: V, p: Priority) extends Treap[K,V] {
    def add[KK >: K,VV >: V](kk: KK, vv: VV)(implicit o: Ordering[KK], pg: PriorityGenerator): Treap[KK,VV] =
      o.comp(kk, k) match {
        case LT => LeftNode(k, v, p, Leaf(kk, vv, pg()))
        case EQ => Leaf(k, vv, p)
        case GT => RightNode(k, v, p, Leaf(kk, vv, pg()))
      }

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] =
      if(o.equiv(kk, k)) Treap.empty[K,V]
      else this

    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      Option(v) filter (_ => o.equiv(kk, k))

    def toList = List((k, v))
  }

  case class LeftNode[K,V](k: K, v: V, p: Priority, l: Treap[K,V]) extends Treap[K,V] {
    def add[KK >: K,VV >: V](kk: KK, vv: VV)(implicit o: Ordering[KK], pg: PriorityGenerator): Treap[KK,VV] =
      o.comp(kk, k) match {
        case LT => LeftNode(k, v, p, l.add(kk, vv)(o, pg))
        case EQ => LeftNode(k, vv, p, l)
        case GT => Node(k, v, p, l, Leaf(kk, vv, pg()))
      }

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] = ???
    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      o.comp(kk, k) match {
        case LT => l.find(kk)
        case EQ => Option(v)
        case GT => None
      }

    def toList = l.toList ++ List((k, v))
  }

  case class RightNode[K,V](k: K, v: V, p: Priority, r: Treap[K,V]) extends Treap[K,V] {
    def add[KK >: K,VV >: V](kk: KK, vv: VV)(implicit o: Ordering[KK], pg: PriorityGenerator): Treap[KK,VV] =
      o.comp(kk, k) match {
        case LT => Node(k, v, p, Leaf(kk, vv, pg()), r)
        case EQ => RightNode(k, vv, p, r)
        case GT => RightNode(k, v, p, r.add(kk, vv)(o, pg))
      }

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] = ???
    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      o.comp(kk, k) match {
        case LT => None
        case EQ => Option(v)
        case GT => r.find(kk)
      }

    def toList = List((k, v)) ++ r.toList
  }

  case class Node[K,V](k: K, v: V, p: Priority, l: Treap[K,V], r: Treap[K,V]) extends Treap[K,V] {
    def add[KK >: K,VV >: V](kk: KK, vv: VV)(implicit o: Ordering[KK], pg: PriorityGenerator): Treap[KK,VV] =
      o.comp(kk, k) match {
        case LT => Node(k, v, p, l.add(kk, vv)(o, pg), r)
        case EQ => Node(k, vv, p, l, r)
        case GT => Node(k, v, p, l, r.add(kk, vv)(o, pg))
      }

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] = ???
    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      o.comp(kk, k) match {
        case LT => l.find(kk)
        case EQ => Option(v)
        case GT => r.find(kk)
      }

    def toList = l.toList ++ List((k, v)) ++ r.toList
  }

  implicit val randomPriorityGenerator = new PriorityGenerator {
    def apply = scala.util.Random.nextInt()
  }


}


object CompareHelper {
  sealed trait Comparison
  case object LT extends Comparison
  case object EQ extends Comparison
  case object GT extends Comparison

  implicit class OrderingPimp[A](val o: Ordering[A]) extends AnyVal {
    def comp(x: A, y: A): Comparison = {
      val c = o.compare(x, y)
      if(c < 0) LT
      else if(c > 0) GT
      else EQ
    }
  }
}
