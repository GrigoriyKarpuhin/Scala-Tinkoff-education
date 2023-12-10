package task1.hierarchy
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import task1._
import task1.hierarchy.TypeClasses._

class TypeClassesSpec extends AnyFlatSpec with Matchers {
  "map" should "map over the tree" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    TreeFunctor.map(tree)(_ * 2) shouldEqual Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
  }

  "apply" should "apply a function to a tree" in {
    val funcTree = Branch(Leaf((a: Int) => a * 2), Leaf((a: Int) => a + 1))
    val valueTree = Branch(Leaf(1), Leaf(2))
    TreeApply.ap(funcTree)(valueTree) shouldEqual Branch(Leaf(2), Leaf(3))
  }

  "applicative" should "create a tree with a single value" in {
    TreeApplicative.pure(42) shouldEqual Leaf(42)
  }

  "flatMap" should "flatMap over the tree" in {
    val tree = Branch(Leaf(1), Leaf(2))
    TreeFlatMap.flatMap(tree)(a => Branch(Leaf(a * 2), Leaf(a + 1))) shouldEqual Branch(
      Branch(Leaf(2), Leaf(2)),
      Branch(Leaf(4), Leaf(3))
    )
  }

  "monad" should "work as applicative and flatMap" in {
    TreeApplicative.pure(228) shouldEqual Leaf(228)
    val tree = Branch(Leaf(0), Leaf(1))
    TreeFlatMap.flatMap(tree)(a => Branch(Leaf(a * 2), Leaf(a + 1))) shouldEqual Branch(
      Branch(Leaf(0), Leaf(1)),
      Branch(Leaf(2), Leaf(2))
    )
  }
}
