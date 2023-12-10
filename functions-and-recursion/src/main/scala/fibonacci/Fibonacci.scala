package fibonacci

import scala.annotation.tailrec

object Fibonacci {
  @tailrec
  def fibonacci(limit: Long, acc1: BigInt = 0, acc2: BigInt = 1): BigInt = {
    if (limit == 0)
      acc1
    else if (limit == 1)
      acc2
    else
      fibonacci(limit - 1, acc2, acc1 + acc2)
  }
}
