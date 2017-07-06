package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def drop[A](as: List[A], n: Int): List[A] = n match {
    case x if x <= 0 => as
    case y if y > 0 => drop(tail(as), n - 1)
  }

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def setHead[A](as: List[A])(a: A): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(a, xs)
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
    }

  def length[A](as : List[A]) : Int = foldRight(as, 0)((x, acc) => acc + 1)
  def length2[A](as : List[A]) : Int = foldLeft(as, 0)((acc, x) => acc + 1)

  def sum2(l: List[Int]) = foldRight(l, 0.0)(_ + _)
  def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)
  def sum3(l: List[Int]) = foldLeft(l, 0.0)(_ + _)
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def reverse[A](as : List[A]) : List[A] = {
    foldRight(as, Nil:List[A])((h,t) => append(t, List(h)))
  }

  def append2[A](a1 : List[A], a2 : List[A]) : List[A] = {
    foldRight(a1, a2)((h, t) => Cons(h, t))
  }

  def concatListOfLists[A](as : List[List[A]]) : List[A] = {
    foldRight(as, Nil:List[A])((l,r) => append(l, r))
  }

  def add1(as: List[Int]) : List[Int] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, add1(xs))
  }

  def doubleToString(ds : List[Double]) : List[String] = ds match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f"${x}", doubleToString(xs))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((x, y) => if(f(x)) Cons(x, y) else y)
    //flatMap(as)(x => if(f(x)) List(x) else Nil)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append2(f(x), flatMap(xs)(f))
  }

  def zip[A](a1: List[A], a2: List[A])(f: (A, A) => A) : List[A] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zip(xs, ys)(f))
  }

  // def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
  //   def go(as: List[A], sub: List[A]) : Boolean = (as, sub) match {
  //     case (_, Nil) => true
  //     case (Nil, _) => false
  //     case (Cons(x, xs), Cons(y, ys)) => x == y && go(xs, ys)
  //   }
  //   go(l, sub) || hasSubsequence(tail(l), sub)
  // }

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)
}
