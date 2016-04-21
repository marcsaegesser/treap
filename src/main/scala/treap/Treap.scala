package treaps

sealed trait Treap[+K,+V] {
  def k: K
  def v: V

  private[treaps] def p: Int

  /** Add/update a key/value pair and return a new Treap. O(log n)
    *
    * If the key already exists its value is updated.
    */
  def add[KK >: K,VV >: V](k: KK, v: VV)(implicit o: Ordering[KK], pg: PriorityGenerator[Int]): Treap[KK,VV] =
    add(k, v, pg())

  /** Remove the specified key and return a new Treap. O(log n)
    */
  def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V]

  /** Look up the value for the specified key returning None if the key is not found. O(lon g)
    */
  def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V]

  /** True if the Treap is empty. O(1)
    */
  def isEmpty: Boolean

  /* A general fold over the Treap.
   *
   * The z function is invoked for Empty Treaps and the n function is
   * invoked for nodes. The node function is passed the left child
   * Treap, the node's key and value and the node's right child,
   * respectively. For purposes of fold a leaf can be considered a node
   * with two Empty children.
   */
  def fold[A](z: => A)(n: (Treap[K,V], K, V, Treap[K,V]) => A): A

  /** Performs a left fold over the Treap.
    *
    * This can be thought of as an in-order traversal of the Treap by keys.
    */
  def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A

  /** Returns a List sorted by key values (in increasing order)
    */
  def toList: List[(K, V)]

  /** The number of elements in the Treap. O(n).
    */
  def size: Int = foldLeft(0)((a, t) => a + 1)

  /** Add/update a key/value pair with the specified priority, p.
    */
  private[treaps]
  def add[KK >: K,VV >: V](k: KK, v: VV, p: Int)(implicit o: Ordering[KK]): Treap[KK,VV]

  /** Restore heap ordering.
    */
  private[treaps]
  def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV]

  /** Rotate left about this node.
    */
  private[treaps]
  def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): Treap[KK,VV]

  /** Rotate right about this node.
    */
  private[treaps]
  def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): Treap[KK,VV]

  /** Remove an Empty child.
    */
  private[treaps]
  def cleanup[KK >: K,VV >: V]: Treap[KK,VV]
}

/** Defines a generator of priority values.
  */
trait PriorityGenerator[A] {
  def apply(): A
}

/** The Default priority generator returns random Int values.
  */
object DefaultPriorityGenerator {
  implicit val randomPriorityGenerator = new PriorityGenerator[Int] {
    def apply: Int = scala.util.Random.nextInt()
  }
}

object Treap extends {
  def apply[K,V](kvps: (K,V)*)(implicit o: Ordering[K], pg: PriorityGenerator[Int]): Treap[K,V] = kvps.toList.foldLeft(Treap.empty[K,V])((a, kvp) => a.add(kvp._1, kvp._2))

  def empty[K,V]: Treap[K,V] = EmptyTreap


  case object EmptyTreap extends Treap[Nothing,Nothing] {
    def k: Nothing = throw new NoSuchElementException("EmptyTreap has no key")
    def v: Nothing = throw new NoSuchElementException("EmptyTreap has no value")

    private[treaps] def p: Int = throw new NoSuchElementException("EmptyTreap has no priority")

    def remove[KK >: Nothing](x: KK)(implicit o: Ordering[KK]): Treap[KK,Nothing] = EmptyTreap

    def find[KK >: Nothing](x: KK)(implicit o: Ordering[KK]): Option[Nothing] = None

    def isEmpty: Boolean = true

    def fold[A](z: => A)(n: (Treap[Nothing,Nothing], Nothing, Nothing, Treap[Nothing,Nothing]) => A): A = z

    def foldLeft[A](z: A)(f: (A, Treap[Nothing,Nothing]) => A): A = z

    def toList = List()

    private[treaps]
    def add[K,V](k: K, v: V, p: Int)(implicit o: Ordering[K]): Treap[K,V] =
      LeafNode[K,V](k, v, p)

    private[treaps]
    def reheap[KK,VV]()(implicit o: Ordering[KK]): treaps.Treap[Nothing,Nothing] = throw new Exception("Cannot perform reheap on an EmptyTreap")

    private[treaps]
    def rotateLeft[KK,VV](implicit o: Ordering[KK]): Treap[Nothing, Nothing] = throw new Exception("Cannot perform rotateLeft on an EmptyTreap")

    private[treaps]
    def rotateRight[KK,VV](implicit o: Ordering[KK]): Treap[Nothing, Nothing] = throw new Exception("Cannot perform rotateRight on an EmptyTreap")

    private[treaps]
    def cleanup[KK,VV]: Treap[KK,VV] = throw new Exception("Cannot perform cleanup on an EmptyTreap")
  }

  import CompareHelper._

  case class LeafNode[K,V](k: K, v: V, p: Int) extends Treap[K,V] {

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] =
      o.comp(kk, k) fold (this, Treap.empty[K,V], this)

    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      Option(v) filter (_ => o.equiv(kk, k))

    def isEmpty: Boolean = false

    def fold[A](z: => A)(n: (Treap[K,V], K, V, Treap[K,V]) => A): A =
      n(EmptyTreap, k, v, EmptyTreap)

    def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A =
      f(z, this)

    def toList = List((k, v))

    private[treaps]
    def add[KK >: K,VV >: V](kk: KK, vv: VV, pp: Int)(implicit o: Ordering[KK]): Treap[KK,VV] =
      o.comp(kk, k) fold (
        LeftNode(k, v, p, LeafNode(kk, vv, pp)).reheap,
        copy(v = vv),
        RightNode(k, v, p, LeafNode(kk, vv, pp)).reheap
      )


    private[treaps]
    def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV] = this
    private[treaps]
    def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] = throw new Exception("Cannot perform rotateLeft on a LeafNode node")
    private[treaps]
    def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] = throw new Exception("Cannot perform rotateRight on a Leaf node")
    private[treaps]
    def cleanup[KK >: K,VV >: V]: Treap[KK,VV] = this
  }

  case class LeftNode[K,V](k: K, v: V, p: Int, l: Treap[K,V]) extends Treap[K,V] {

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] =
      o.comp(kk, k) fold (
        LeftNode(k, v, p, l.remove(kk)).cleanup,
        l,
        this
      )

    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      o.comp(kk, k) fold (l.find(kk), Option(v), None)

    def isEmpty: Boolean = false

    def fold[A](z: => A)(n: (Treap[K,V], K, V, Treap[K,V]) => A): A =
      n(l, k, v, EmptyTreap)

    def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A =
      f(l.foldLeft(z)(f), this)

    def toList = l.toList ++ List((k, v))

    private[treaps]
    def add[KK >: K,VV >: V](kk: KK, vv: VV, pp: Int)(implicit o: Ordering[KK]): Treap[KK,VV] =
      o.comp(kk, k) fold (
        LeftNode(k, v, p, l.add(kk, vv, pp)).reheap,
        copy(v = vv),
        FullNode(k, v, p, l, LeafNode(kk, vv, pp)).reheap
      )

    private[treaps]
    def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV] =
      (p comp l.p) fold (rotateRight(o), this, this)

    private[treaps]
    def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] = throw new Exception("Cannot perform rotateLeft on a Left node")

    private[treaps]
    def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] =
      l match {
        case LeafNode(lk, lv, lp)          => RightNode(lk, lv, lp, LeafNode(k, v, p))
        case LeftNode(lk, lv, lp, ll)      => FullNode(lk, lv, lp, ll, LeafNode(k, v, p))
        case RightNode(lk, lv, lp, lr)     => RightNode(lk, lv, lp, LeftNode(k, v, p, lr))
        case FullNode(lk, lv, lp, ll, lr)  => FullNode(lk, lv, lp, ll, LeftNode(k, v, p, lr))
        case EmptyTreap                    => throw new IllegalStateException("Treap contains EmptyTreap")
      }

    private[treaps]
    def cleanup[KK >: K,VV >: V]: Treap[KK,VV] = if(l.isEmpty) LeafNode(k, v, p) else this

  }

  case class RightNode[K,V](k: K, v: V, p: Int, r: Treap[K,V]) extends Treap[K,V] {

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] =
      o.comp(kk, k) fold (
        this,
        r,
        RightNode(k, v, p, r.remove(kk)).cleanup
      )

    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      o.comp(kk, k) fold (None, Option(v), r.find(kk))

    def isEmpty: Boolean = false

    def fold[A](z: => A)(n: (Treap[K,V], K, V, Treap[K,V]) => A): A =
      n(EmptyTreap, k, v, r)

    def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A =
      r.foldLeft(f(z, this))(f)

    def toList = List((k, v)) ++ r.toList

    private[treaps]
    def add[KK >: K,VV >: V](kk: KK, vv: VV, pp: Int)(implicit o: Ordering[KK]): Treap[KK,VV] =
      o.comp(kk, k) fold (
        FullNode(k, v, p, LeafNode(kk, vv, pp), r).reheap,
        copy(v = vv),
        RightNode(k, v, p, r.add(kk, vv, pp)).reheap
      )


    private[treaps]
    def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV] =
      (p comp r.p) fold (rotateLeft(o), this, this)

    private[treaps]
    def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] =
      r match {
        case LeafNode(rk, rv, rp)         => LeftNode(rk, rv, rp, LeafNode(k, v, p))
        case LeftNode(rk, rv, rp, rl)     => LeftNode(rk, rv, rp, RightNode(k, v, p, rl))
        case RightNode(rk, rv, rp, rr)    => FullNode(rk, rv, rp, LeafNode(k, v, p), rr)
        case FullNode(rk, rv, rp, rl, rr) => FullNode(rk, rv, rp, RightNode(k, v, p, rl), rr)
        case EmptyTreap                   => throw new IllegalStateException("Treap contains EmptyTreap")
      }

    private[treaps]
    def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] = throw new Exception("Cannot perform rotateRight on a Right node")

    private[treaps]
    def cleanup[KK >: K,VV >: V]: Treap[KK,VV] = if(r.isEmpty) LeafNode(k, v, p) else this
  }

  case class FullNode[K,V](k: K, v: V, p: Int, l: Treap[K,V], r: Treap[K,V]) extends Treap[K,V] {

    def remove[KK >: K](kk: KK)(implicit o: Ordering[KK]): Treap[KK,V] =
      o.comp(kk, k) fold (
        FullNode(k, v, p, l.remove(kk), r).cleanup,
        ((l.p comp r.p) fold (rotateLeft(o), rotateRight(o), rotateRight(o)) remove(kk)).cleanup,
        FullNode(k, v, p, l, r.remove(kk)).cleanup
      )

    def find[KK >: K](kk: KK)(implicit o: Ordering[KK]): Option[V] =
      o.comp(kk, k) fold (l.find(kk), Option(v), r.find(kk))

    def isEmpty: Boolean = false

    def fold[A](z: => A)(n: (Treap[K,V], K, V, Treap[K,V]) => A): A =
      n(l, k, v, r)

    def foldLeft[A](z: A)(f: (A, Treap[K,V]) => A): A =
      r.foldLeft(f(l.foldLeft(z)(f), this))(f)

    def toList = l.toList ++ List((k, v)) ++ r.toList

    private[treaps]
    def add[KK >: K,VV >: V](kk: KK, vv: VV, pp: Int)(implicit o: Ordering[KK]): Treap[KK,VV] =
      o.comp(kk, k) fold (
        FullNode(k, v, p, l.add(kk, vv, pp), r).reheap,
        copy(v = vv),
        FullNode(k, v, p, l, r.add(kk, vv, pp)).reheap
      )

    private[treaps]
    def reheap[KK >: K,VV >: V]()(implicit o: Ordering[KK]): Treap[KK,VV] =
      (l.p comp r.p) fold (
        (p comp r.p) fold (rotateLeft(o), this, this),
        this,
        (p comp l.p) fold (rotateRight(o), this, this)
      )

    private[treaps]
    def rotateLeft[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] =
      r match {
        case LeafNode(rk, rv, rp)         => LeftNode(rk, rv, rp, LeftNode(k, v, p, l))
        case LeftNode(rk, rv, rp, rl)     => LeftNode(rk, rv, rp, FullNode(k, v, p, l, rl))
        case RightNode(rk, rv, rp, rr)    => FullNode(rk, rv, rp, LeftNode(k, v, p, l), rr)
        case FullNode(rk, rv, rp, rl, rr) => FullNode(rk, rv, rp, FullNode(k, v, p, l, rl), rr)
        case EmptyTreap                   => throw new IllegalStateException("Treap contains EmptyTreap")
      }

    private[treaps]
    def rotateRight[KK >: K,VV >: V](implicit o: Ordering[KK]): treaps.Treap[KK,VV] =
      l match {
        case LeafNode(lk, lv, lp)         => RightNode(lk, lv, lp, RightNode(k, v, p, r))
        case LeftNode(lk, lv, lp, ll)     => FullNode(lk, lv, lp, ll, RightNode(k, v, p, r))
        case RightNode(lk, lv, lp, lr)    => RightNode[KK,VV](lk, lv, lp, FullNode(k, v, p, lr, r))
        case FullNode(lk, lv, lp, ll, lr) => FullNode(lk, lv, lp, ll, FullNode(k, v, p, lr, r))
        case EmptyTreap                   => throw new IllegalStateException("Treap contains EmptyTreap")
      }

    private[treaps]
    def cleanup[KK >: K,VV >: V]: Treap[KK,VV] =
      this match {
        case FullNode(k, v, p, EmptyTreap, r) => RightNode(k, v, p, r)
        case FullNode(k, v, p, l, EmptyTreap) => LeftNode(k, v, p, l)
        case FullNode(_, _, _, _, _)          => this
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
