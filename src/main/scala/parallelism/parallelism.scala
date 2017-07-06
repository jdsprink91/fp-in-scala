package parallelism

import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = es => {
    es.submit(new Callable[A]() {
      def call = a
    })
  }

  // waits for an executor service to run
  def fork[A](a: => Par[A]): Par[A] = es => a(es)

  // we can just hand in a plain ol value and it returns back
  // something that is an executor service
  def async[A](a: => A): Par[A] = fork(unit(a))

  // runs everything because we gave it an executor service
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val result = f(a(es) get, b(es) get)
    es.submit(new Callable[C]() {
      def call = result
    })
  }

  def map[A,B](fa: Par[A])(f: A => B): Par[B] = {
    map2(fa, unit(()))((a,_) => f(a))
  }

  def asyncF[A,B](f: A => B): A => Par[B] = a => async(f(a))

  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = es => {
    es.submit(new Callable[List[B]]() {
      def call = l map f
    })
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = es => {
    es.submit(new Callable[List[A]]() {
      def call = l filter f
    })
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] = es => {
    val result = l map (_(es) get)
    es.submit(new Callable[List[A]] {
      def call = result
    })
  }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = es => {
    // if(a(es) get) ifTrue(es)
    // else ifFalse(es)
    flatMap(a)(if (_) ifTrue else ifFalse)(es)
  }

  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    //choices(a(es) get)(es)
    flatMap(a)(choices(_))(es)
  }

  def choiceMap[A,B](a: Par[A])(choices: Map[A,Par[B]]): Par[B] = es => {
    //choices(a(es) get)(es)
    flatMap(a)(choices(_))(es)
  }

  def flatMap[A,B](a: Par[A])(choices: A => Par[B]): Par[B] = es => {
    choices(a(es) get)(es)
  }

  def join[A](a: Par[Par[A]]): Par[A] = es => a(es).get()(es)
}
