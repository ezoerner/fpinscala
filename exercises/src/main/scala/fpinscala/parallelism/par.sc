import java.util.concurrent.ExecutorService

/** =Exercise 7.1= */
def map2[A,B,C](p1: Par[A], p2: Par[B], f: (A, B) ⇒ C): Par[C] = ???


/** =Exercise 7.2= */
class Par[A](a: ⇒A, toFork: Boolean = false) {
  def map2[B,C](b: Par[B])(f: (A,B) ⇒ C): Par[C] = ???
  def run(implicit executorService: ExecutorService): A = ???
  def fork: Par[A] = new Par(a, true)
}


object Par {
  def unit[A](a: A): Par[A] = new Par(a)
  def lazyUnit[A](a: ⇒A): Par[A] = new Par(a, true)
}
