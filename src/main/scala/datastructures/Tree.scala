package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree : Tree[A]) : Int = tree match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree : Tree[Int]) : Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree : Tree[A]) : Int = tree match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree : Tree[A])(f: A => B) : Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B) : B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeFold[A](tree : Tree[A]) : Int = {
    fold(tree)(x => 1)((y, z) => 1 + y + z)
  }

  def maximumFold(tree : Tree[Int]) : Int = {
    fold(tree)(x => x)((y, z) => y max z)
  }

  def depthFold[A](tree : Tree[A]) : Int = {
    fold(tree)(x => 1)((y, z) => 1 + y max z)
  }

  def mapFold[A, B](tree : Tree[A])(f : A => B) : Tree[B] = {
    fold(tree)(x => Leaf(f(x)) : Tree[B])((y, z) => Branch(y, z))
  }
}
