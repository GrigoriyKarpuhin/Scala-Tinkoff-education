package task2
import cats.Eval
object EvalTricks {

  def fib(n: Int): BigInt = {
    def helper(n: Int, a: BigInt, b: BigInt): Eval[BigInt] = {
      if (n <= 0) Eval.now(a)
      else {
        Eval.defer(helper(n - 1, b, a + b))
      }
    }
    helper(n, 0, 1).value
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = ???
}
