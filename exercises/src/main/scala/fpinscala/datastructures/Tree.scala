package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  // “Write a function size that counts the number of nodes (leaves and branches) in a tree.”
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // Exercise 3.26
  // “Write a function maximum that returns the maximum element in a Tree[Int].
  // (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)”
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  // Exercise 3.27
  // “Write a function depth that returns the maximum path length from the root of a tree to any leaf.”
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  // Exercise 3.28
  // “Write a function map, analogous to the method of the same name on List,
  // that modifies each element in a tree with a given function.”
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 3.29
  //“Generalize size, maximum, depth, and map, writing a new function fold that
  // abstracts over their similarities. Reimplement them in terms of this more general function.
  // Can you draw an analogy between this fold function and the left and right folds for List?”
/*
  def fold[A,B](tree: Tree[A])(z: B)(f: (A, B) => B): B = tree match {
    case Leaf(v) => f(v, z)
    case Branch(left, right) => fold(right)(fold(left)(z)(f))(f)
  }
*/
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = ???

  def sizeFold[A](tree: Tree[A]): Int = ??? //fold(tree)(0)( (a, b) =>  b + 1)
}