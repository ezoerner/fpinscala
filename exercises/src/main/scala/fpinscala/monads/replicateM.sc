import fpinscala.monads.Monad

val ml: Monad[List] = Monad.listMonad

ml.replicateM(3, List(5,6))

ml.sequence(List.fill(3)(List(5,6)))

ml.sequence(List(List(5, 6), List(5, 6), List(5, 6)))

ml.traverse(List(List(5, 6), List(5, 6), List(5, 6)))(identity)

val zero: List[List[Int]] = ml.unit(Nil)

List(List(5, 6), List(5, 6), List(5, 6)).foldLeft(zero) { (acc: List[List[Int]], m: List[Int]) ⇒
  ml.map2[Int,List[Int],List[Int]](m, acc)( _ :: _)
}

type A = Int
type B = List[Int]
type C = List[Int]


val f: (A, B) ⇒ C = _ :: _

List(List(5, 6), List(5, 6), List(5, 6)).foldLeft(zero) { (acc: List[List[Int]], m: List[Int]) ⇒
  for {
    a ← m
    b ← acc
  } yield f(a, b)
  //ml.flatMap(m: List[Int])((a: Int) ⇒ ml.map(acc: List[List[Int]])((b: List[Int]) ⇒ f(a, b)))
}




val acc1 = zero
val m1 = List(5,6)

ml.map2[Int,List[Int],List[Int]](m1, acc1)(_ :: _)


val acc2 = List(List(5), List(6))
val m2 = List(5,6)

ml.map2[Int,List[Int],List[Int]](m2, acc2)(_ :: _)

val acc3 = List(List(5, 5), List(5, 6), List(6, 5), List(6, 6))
val m3 = List(5,6)

ml.map2[Int,List[Int],List[Int]](m3, acc3)(_ :: _)
