package laziness

import Stream._
import scala.collection.immutable.List

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList : List[A] = {
    uncons match {
      case None => Nil
      case Some((hd, tl)) => hd :: tl.toList
    }
  }

  def take(n : Int) : Stream[A] = {
    // uncons match {
    //   case None => Stream.empty
    //   case Some((hd, tl)) => if(n > 0) cons(hd, tl.take(n - 1)) else Stream.empty
    // }
    unfold((n, this))(s => (s._1, s._2.uncons) match {
      case(x, _) if x <= 0 => None
      case(_, None) => None
      case(x, Some((hd, tl))) => Some(hd, (x - 1, tl))
    })
  }

  def takeWhile(p : A => Boolean) : Stream[A] = {
    // uncons match {
    //   case None => Stream()
    //   case Some((hd, tl)) => if(p(hd)) cons(hd, tl.takeWhile(p)) else Stream()
    // }
    // foldRight(Stream[A]())((a, b) => if(p(a)) cons(a, b) else Stream.empty)
    unfold(this)(s => s.uncons match {
      case None => None
      case Some((hd, tl)) => if(p(hd)) Some((hd, tl)) else None
    })
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    uncons match {
      case None => z
      case Some((h, t)) => f(h, t.foldRight(z)(f))
    }
  }

  def forAll(p : A => Boolean) : Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def map[B](f: A => B) : Stream[B] = {
    //foldRight(Stream.empty[B])((a, b) => cons(f(a), b))
    unfold(this)(s => s.uncons match {
      case None => None
      case Some((h, t)) => Some(f(h), t)
    })
  }

  def filter(f: A => Boolean) : Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if(f(a)) cons(a, b) else b)
  }

  def append[B >: A](s : Stream[B]) : Stream[B] = {
    foldRight(s)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]) : Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))
  }

  def tails: Stream[Stream[A]] = {
    unfold(this)(state => state.uncons match {
      case None => None
      case Some((hd, tl)) => Some(this, tl)
    })
  }
}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    //cons(a, constant(a))
    unfold(a)(s => Some(s, s))
  }

  def from(n: Int): Stream[Int] = {
    // cons(n, from(n + 1))
    unfold(n)(s => Some(s, s + 1))
  }

  def fibs() : Stream[Int] = {
  //   def go(prev: => Int, curr: => Int): Stream[Int] = {
  //     cons(prev, go(curr, prev + curr))
  //   }
  //   go(0, 1)
    unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))
  }

  def zip[A](s1: Stream[A], s2: Stream[A])(f: (A, A) => A) = {
    unfold((s1, s2))(state => (state._1.uncons, state._2.uncons) match {
      case (None, _) => None
      case (_, None) => None
      case (Some((hd1, tl1)), Some((hd2, tl2))) => Some(f(hd1, hd2), (tl1, tl2))
    })
  }

  def zipAll[A](s1: Stream[A], s2: Stream[A])(f: (A, A) => A) = {
    unfold((s1, s2))(state => (state._1.uncons, state._2.uncons) match {
      case (None, None) => None
      case (None, Some((hd, tl))) => Some(hd, (Stream.empty, tl))
      case (Some((hd, tl)), None) => Some(hd, (tl, Stream.empty))
      case (Some((hd1, tl1)), Some((hd2, tl2))) => Some(f(hd1, hd2), (tl1, tl2))
    })
  }

  def zipMerge[A, B, C](s1: Stream[A], s2: Stream[B])(f: (A, B) => C) = {
    unfold((s1, s2))(state => (state._1.uncons, state._2.uncons) match {
      case (None, _) => None
      case (_, None) => None
      case (Some((hd1, tl1)), Some((hd2, tl2))) => Some(f(hd1, hd2), (tl1, tl2))
    })
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((hd, tl)) => cons(hd, unfold(tl)(f))
    }
  }
}
