package treaps

import scala.util.Random
import org.scalameter.api._
import org.scalameter.picklers.Implicits

trait TreapBenchmarks extends Bench.OfflineRegressionReport {
  import DefaultPriorityGenerator._
  val sizes = Gen.range("size")(10000, 150000, 10000)

  val lists = for {
    size <- sizes
  } yield List.fill(size)(Random.nextInt())

  val treaps = for {
    ks <- lists
  } yield Treap((ks zip ks):_*)

  val treapsAndKeys = for {
    ks <- lists
  } yield (ks, Treap((ks zip ks):_*))

  val treapsAndList = for {
    t <- treaps
    l  = List.fill(10000)(Random.nextInt())
  } yield (t, l)

  performance of "Treap" in {
    measure method "add" in {
      using(treapsAndList) in { case (t, l) =>
        l.foldLeft(t)((a, i) => a.add(i, i))
      }
    }
    measure method "find" in {
      using(treapsAndKeys) in { case (ks, t) =>
        ks.take(10000) foreach { t.find(_) }
      }
    }
  }
}

class TreapSuite extends Bench.Group {
  performance of "Treap" config {
    reports.resultDir -> "target/benchmarks/treap"
  } in {
    include( new TreapBenchmarks {} )
  }
}
