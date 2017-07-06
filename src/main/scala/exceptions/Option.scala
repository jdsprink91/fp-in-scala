package exceptions

import scala.{Option => _}
import java.util.regex._

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (x => Some(x)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if(f(a)) Some(a) else None)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  def pattern(s: String): Option[Pattern] = {
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }
  }

  def mkMatcher(pat: String): Option[String => Boolean] = {
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)
  }

  def bothMatch(pat1 : String, pat2 : String, s : String) : Option[Boolean] = {
    map2(mkMatcher(pat1), mkMatcher(pat2))((f1, f2) => f1(s) && f2(s))
  }

  /*
    So what's happening here is
    1) if the list matches Nil return some Nil
    2) If the list is of the form Cons(head, tail) then
      2a) take the Some(head) and flat map it
      2b) the flat map function exposes head (hh)
      2c) the sequence(t) recurs over the rest of the option list and
          returns an Option(t)
      2d) map the sequence(t) which exposes t
      2e) prepend the exposed head with the exposed tail
      2f) flatMap returns an option, so then that whole thing is wrapped
          in an option
  */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (x => hh :: x))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence(a map f)
  }
}
