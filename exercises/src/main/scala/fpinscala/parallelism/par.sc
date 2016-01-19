
case class Par[A]()

/** =Exercise 7.1= */
def map2[A,B,C](p1: Par[A], p2: Par[B], f: (A, B) ⇒ C): Par[C]
