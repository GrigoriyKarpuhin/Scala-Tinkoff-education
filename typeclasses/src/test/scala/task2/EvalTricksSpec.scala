package task2
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EvalTricksSpec extends AnyFlatSpec with Matchers {
  "fib" should "return the correct Fibonacci number" in {
    EvalTricks.fib(0) should be(0)
    EvalTricks.fib(1) should be(1)
    EvalTricks.fib(2) should be(1)
    EvalTricks.fib(3) should be(2)
    EvalTricks.fib(10) should be(55)
    EvalTricks.fib(50) should be(BigInt("12586269025"))
    EvalTricks.fib(100) should be(BigInt("354224848179261915075"))
  }
}
