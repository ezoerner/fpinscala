package fpinscala.exercises.datastructures

import scala.math.max
import Function.const

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (l.depth `max` r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(a)      => f(a)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int = fold(const(1), 1 + _ + _)

  def depthViaFold: Int = fold(const(0), (ld, rd) => 1 + (ld `max` rd))

  def mapViaFold[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)), (lb, rb) => Branch(lb, rb))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int])
    // this is fucked up, it should return an Option[Int]
    def firstPositive: Int = t match
      case Leaf(n) => n
      case Branch(l, r) =>
        val lpos = l.firstPositive
        if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int])
    def maximum: Int = t match
      case Leaf(n)      => n
      case Branch(l, r) => l.maximum `max` r.maximum

  extension (t: Tree[Int])
    def maximumViaFold: Int =
      t.fold(identity, (a, acc) => a `max` acc)
