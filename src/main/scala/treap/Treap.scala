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

  def empty[K,V]: Treap[K,V] = EmptyTreap

  case object EmptyTreap extends Treap[Nothing,Nothing] {
    def add[K,V](k: K, v: V)(implicit o: Ordering[K], pg: PriorityGenerator): Treap[K,V] =
      LeafTreap[K,V](k, v, pg())

    def remove[KK >: Nothing](x: KK)(implicit o: Ordering[KK]): Treap[KK,Nothing] = throw new NoSuchElementException("remove on empty Treap")

    def find[KK >: Nothing](x: KK)(implicit o: Ordering[KK]): Option[Nothing] = None

    def toList = List()
  }

  import CompareHelper._

  case class LeafTreap[K,V](k: K, v: V, p: Priority) extends Treap[K,V] {
    def add[KK >: K,VV >: V](kk: KK, vv: VV)(implicit o: Ordering[KK], pg: PriorityGenerator): Treap[KK,VV] =
      o.comp(kk, k) match {
        case LT => Node(k, v, p, Some(LeafTreap(kk, vv, pg())), None)
        case EQ => LeafTreap(k, vv, p)
        case GT => Node(k, v, p, None, Some(LeafTreap(kk, vv, pg())))
      }

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] =
      if(o.equiv(kk, k)) Treap.empty[K,V]
      else this

    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      Option(v) filter (_ => o.equiv(kk, k))

    def toList = List((k, v))
  }

  case class Node[K,V](k: K, v: V, p: Priority, l: Option[Treap[K,V]], r: Option[Treap[K,V]]) extends Treap[K,V] {
    def add[KK >: K,VV >: V](kk: KK, vv: VV)(implicit o: Ordering[KK], pg: PriorityGenerator): Treap[KK,VV] =
      o.comp(kk, k) match {
        case LT if l.isDefined => Node(k, v, p, l map (_.add(kk, vv)(o, pg)), r)
        case LT if l.isEmpty   => Node(k, v, p, Some(LeafTreap(kk, vv, pg())), r)
        case EQ                => Node(k, vv, p, l, r)
        case GT if r.isDefined => Node(k, v, p, l, r.map (_.add(kk, vv)(o, pg)))
        case GT if r.isEmpty   => Node(k, v, p, l, Some(LeafTreap(kk, vv, pg())))
      }

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] = ???
    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] = ???

    def toList = l.map(_.toList).getOrElse(List()) ++ List((k, v)) ++ r.map(_.toList).getOrElse(List())
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
