package treaps

sealed trait Treap[+K,+V] {
  def k: K
  def v: V
  private[treaps] def p: PriorityGenerator.Priority

  def add[KK >: K,VV >: V](k: KK, v: VV)(implicit o: Ordering[KK], pg: PriorityGenerator): Treap[KK,VV] =
    add(k, v, pg())

  def remove[KK >: K](x: KK)(implicit o: Ordering[KK]): Treap[KK,V]
  def find[KK >: K](x: KK)(implicit o: Ordering[KK]): Option[V]

  def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A

  def toList: List[(K, V)]

  // private[treaps]
  def add[KK >: K,VV >: V](k: KK, v: VV, p: PriorityGenerator.Priority)(implicit o: Ordering[KK]): Treap[KK,VV]
  // private[treaps]
  def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV]
  // private[treaps]
  def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): Treap[KK,VV]
  // private[treaps]
  def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): Treap[KK,VV]
}

trait PriorityGenerator {
  import PriorityGenerator._

  def apply(): Priority
}

object PriorityGenerator {
  type Priority = Int

  implicit val randomPriorityGenerator = new PriorityGenerator {
    def apply = scala.util.Random.nextInt()
  }
}

object Treap extends TreapImpls {
  import PriorityGenerator._

  def apply[K,V](kvps: (K,V)*)(implicit o: Ordering[K]): Treap[K,V] = kvps.toList.foldLeft(Treap.empty[K,V])((a, kvp) => a.add(kvp._1, kvp._2))

  def empty[K,V]: Treap[K,V] = EmptyTreap

}

trait TreapImpls {
  import PriorityGenerator._

  case object EmptyTreap extends Treap[Nothing,Nothing] {
    def k: Nothing = throw new NoSuchElementException("EmptyTreap has no key")
    def v: Nothing = throw new NoSuchElementException("EmptyTreap has no value")
    private[treaps] def p: Priority = throw new NoSuchElementException("EmptyTreap has no priority")

    def remove[KK >: Nothing](x: KK)(implicit o: Ordering[KK]): Treap[KK,Nothing] = throw new NoSuchElementException("remove on empty Treap")

    def find[KK >: Nothing](x: KK)(implicit o: Ordering[KK]): Option[Nothing] = None

    def foldLeft[A](z: A)(f: (A, Treap[Nothing,Nothing]) => A): A = z

    def toList = List()

    /*private[treaps]*/ def add[K,V](k: K, v: V, p: Priority)(implicit o: Ordering[K]): Treap[K,V] =
      Leaf[K,V](k, v, p)

    // private[treaps]
    def reheap[KK,VV]()(implicit o: Ordering[KK]): treaps.Treap[Nothing,Nothing] = EmptyTreap
    // private[treaps]
    def rotateLeft[KK,VV](implicit o: Ordering[KK]): Treap[Nothing, Nothing] = EmptyTreap
    // private[treaps]
    def rotateRight[KK,VV](implicit o: Ordering[KK]): Treap[Nothing, Nothing] = EmptyTreap
  }

  import CompareHelper._

  case class Leaf[K,V](k: K, v: V, p: Priority) extends Treap[K,V] {

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] =
      o.comp(kk, k) fold (this, Treap.empty[K,V], this)

    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      Option(v) filter (_ => o.equiv(kk, k))

    def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A =
      f(z, this)

    def toList = List((k, v))

    // private[treaps]
    def add[KK >: K,VV >: V](kk: KK, vv: VV, pp: Priority)(implicit o: Ordering[KK]): Treap[KK,VV] =
      o.comp(kk, k) fold (
        LeftNode(k, v, p, Leaf(kk, vv, pp)).reheap,
        copy(v = vv),
        RightNode(k, v, p, Leaf(kk, vv, pp)).reheap
      )


    // private[treaps]
    def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV] = this
    // private[treaps]
    def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] = this
    // private[treaps]
    def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] = this
  }

  case class LeftNode[K,V](k: K, v: V, p: Priority, l: Treap[K,V]) extends Treap[K,V] {

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] =
      o.comp(kk, k) fold (
        l.remove(kk),
        ???,
        this
      )

    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      o.comp(kk, k) fold (l.find(kk), Option(v), None)

    def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A =
      f(l.foldLeft(z)(f), this)

    def toList = l.toList ++ List((k, v))

    // private[treaps]
    def add[KK >: K,VV >: V](kk: KK, vv: VV, pp: Priority)(implicit o: Ordering[KK]): Treap[KK,VV] =
      o.comp(kk, k) fold (
        LeftNode(k, v, p, l.add(kk, vv, pp)).reheap,
        copy(v = vv),
        Node(k, v, p, l, Leaf(kk, vv, pp)).reheap
      )


    // private[treaps]
    def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV] =
      (p comp l.p) fold (rotateRight(o), this, this)
    // private[treaps]
    def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] = this

    // private[treaps]
    def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] =
      l match {
        case Leaf(lk, lv, lp)          => RightNode(lk, lv, lp, Leaf(k, v, p))
        case LeftNode(lk, lv, lp, ll)  => Node(lk, lv, lp, ll, Leaf(k, v, p))
        case RightNode(lk, lv, lp, lr) => RightNode(lk, lv, lp, LeftNode(k, v, p, lr))
        case Node(lk, lv, lp, ll, lr)  => Node(lk, lv, lp, ll, LeftNode(k, v, p, lr))
        case _                         => ???
      }
  }

  case class RightNode[K,V](k: K, v: V, p: Priority, r: Treap[K,V]) extends Treap[K,V] {

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] = ???
    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      o.comp(kk, k) fold (None, Option(v), r.find(kk))

    def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A =
      r.foldLeft(f(z, this))(f)

    def toList = List((k, v)) ++ r.toList

    // private[treaps]
    def add[KK >: K,VV >: V](kk: KK, vv: VV, pp: Priority)(implicit o: Ordering[KK]): Treap[KK,VV] =
      o.comp(kk, k) fold (
        Node(k, v, p, Leaf(kk, vv, pp), r).reheap,
        copy(v = vv),
        RightNode(k, v, p, r.add(kk, vv, pp)).reheap
      )


    // private[treaps]
    def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV] =
      (p comp r.p) fold (rotateLeft(o), this, this)

    // private[treaps]
    def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] =
      r match {
        case Leaf(rk, rv, rp)          => LeftNode(rk, rv, rp, Leaf(k, v, p))
        case LeftNode(rk, rv, rp, rl)  => LeftNode(rk, rv, rp, RightNode(k, v, p, rl))
        case RightNode(rk, rv, rp, rr) => Node(rk, rv, rp, Leaf(k, v, p), rr)
        case Node(rk, rv, rp, rl, rr)  => Node(rk, rv, rp, RightNode(k, v, p, rl), rr)
        case _ => ???
      }

    // private[treaps]
    def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] = this
  }

  case class Node[K,V](k: K, v: V, p: Priority, l: Treap[K,V], r: Treap[K,V]) extends Treap[K,V] {

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] = ???

    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      o.comp(kk, k) fold (l.find(kk), Option(v), r.find(kk))

    def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A =
      r.foldLeft(f(l.foldLeft(z)(f), this))(f)

    def toList = l.toList ++ List((k, v)) ++ r.toList

    // private[treaps]
    def add[KK >: K,VV >: V](kk: KK, vv: VV, pp: Priority)(implicit o: Ordering[KK]): Treap[KK,VV] =
      o.comp(kk, k) fold (
        Node(k, v, p, l.add(kk, vv, pp), r).reheap,
        copy(v = vv),
        Node(k, v, p, l, r.add(kk, vv, pp)).reheap
      )

    // private[treaps]
    def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV] =
      (l.p comp r.p) fold (
        (p comp r.p) fold (rotateLeft(o), this, this),
        this,
        (p comp l.p) fold (rotateRight(o), this, this)
      )

    // private[treaps]
    def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] =
      r match {
        case Leaf(rk, rv, rp)          => LeftNode(rk, rv, rp, LeftNode(k, v, p, l))
        case LeftNode(rk, rv, rp, rl)  => LeftNode(rk, rv, rp, Node(k, v, p, l, rl))
        case RightNode(rk, rv, rp, rr) => Node(rk, rv, rp, LeftNode(k, v, p, l), rr)
        case Node(rk, rv, rp, rl, rr)  => Node(rk, rv, rp, Node(k, v, p, l, rl), rr)
        case _                         => throw new IllegalStateException("Treap contains EmptyTreap")
      }

    // private[treaps]
    def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] =
      l match {
        case Leaf(lk, lv, lp)          => RightNode(lk, lv, lp, RightNode(k, v, p, r))
        case LeftNode(lk, lv, lp, ll)  => Node(lk, lv, lp, ll, RightNode(k, v, p, r))
        case RightNode(lk, lv, lp, lr) => RightNode[KK,VV](lk, lv, lp, Node(k, v, p, lr, r))
        case Node(lk, lv, lp, ll, lr)  => Node(lk, lv, lp, ll, Node(k, v, p, lr, r))
        case _                         => throw new IllegalStateException("Treap contains EmptyTreap")
      }
  }
}


object CompareHelper {
  sealed trait Comparison {
    def fold[A](eq: => A, ne: => A): A
    def fold[A](lt: => A, eq: => A, gt: => A): A
  }
  case object LT extends Comparison {
    def fold[A](eq: => A, ne: => A): A = ne
    def fold[A](lt: => A, eq: => A, gt: => A): A = lt
  }
  case object EQ extends Comparison {
    def fold[A](eq: => A, ne: => A): A = eq
    def fold[A](lt: => A, eq: => A, gt: => A): A = eq
  }
  case object GT extends Comparison {
    def fold[A](eq: => A, ne: => A): A = ne
    def fold[A](lt: => A, eq: => A, gt: => A): A = gt
  }

  import Ordering.Implicits._

  implicit class ComparePimp[A : Ordering](val x: A) {
    def comp(y: A): Comparison =
      if(x < y) LT
      else if(x > y) GT
      else EQ
  }

  implicit class OrderingPimp[A](val o: Ordering[A]) extends AnyVal {
    def comp(x: A, y: A): Comparison = {
      val c = o.compare(x, y)
      if(c < 0) LT
      else if(c > 0) GT
      else EQ
    }
  }
}
