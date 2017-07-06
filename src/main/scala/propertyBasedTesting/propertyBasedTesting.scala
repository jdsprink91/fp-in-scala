package propertyBasedTesting

import purelyFunctionalState.RNG
import purelyFunctionalState.RNG.State
import purelyFunctionalState.RNG.Rand
import laziness.Stream

// trait Prop {
//   def check: Boolean
//   def &&(p: Prop): Prop = new Prop {
//     def check = this.check && p.check
//   }
// }

trait Prop { def check: Either[String,Int] }

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

case class Gen[+A](sample: State[RNG,A], exhaustive: Stream[Option[A]]) {
  def unit[A](a: => A): Gen[A] = {
    Gen(State(RNG => (a, RNG)), Stream(Option(a)))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG => {
      val (i, rng2) = RNG.nextInt
      (i % 2 == 0, rng2)
    }), Stream(Option(true), Option(false)))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    def helper(start: Int, stop: Int): Stream[Option[Int]] = {
      if(start == stop) Stream()
      else Stream.cons(Option(start), helper(start + 1, stop))
    }

    Gen(State(RNG => {
      val (i, rng2) = RNG.nextInt
      (i / (stopExclusive - start) + start, rng2)
    }), helper(start, stopExclusive))
  }

  def uniform: Gen[Double] = {
    Gen(State(RNG.double), Stream(None))
  }

  def choose(i: Double, j: Double): Gen[Double] = {
    Gen(State(RNG => {
      val (d, rng2) = RNG.nextInt
      val dd = i.toDouble
      (dd / (j - i) + i, rng2)
    }), Stream(None))
  }

  def map[B](f: A => B): Gen[B] = {
    //Gen(State(RNG.map(this.sample.run)(f)), this.exhaustive map (x => x map f))
    flatMap(a => {
      Gen(State(RNG => (f(a), RNG)), Stream(Option(f(a))))
    })
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(State(RNG.map2(this.sample.run, g.sample.run)(f)),
        Stream.zipMerge(this.exhaustive, g.exhaustive)((a, b) => (a, b) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(a), Some(b)) => Option(f(a, b))
        }))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    def helper(s: Stream[Option[A]], rng: RNG): Stream[Option[B]] = {
      s.uncons match {
        case None => Stream.empty
        case Some((hd, tl)) => {
          hd match {
            case None => Stream.empty
            case Some(a) => {
              var result = f(a)
              var (i, rng2) = result.sample.run(rng)
              Stream.cons(Option(i), helper(tl, rng2))
            }
          }
        }
      }
    }

    Gen(State(RNG.flatMap(this.sample.run)(a => f(a).sample.run)),
        helper(this.exhaustive, RNG.simple(5)))
  }
}

object Gen {
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    def helper(n: Int, g: Gen[A], r: RNG): List[A] = {
      if(n == 0) Nil
      else {
        var state: State[RNG, A] = g.sample
        var (i, rng2) = state.run(r)
        i :: helper(n - 1, g, rng2)
      }
    }
    var result = helper(n, g, RNG.simple(5))
    Gen(State(RNG => (result, RNG)), Stream(Option(result)))
  }
}
