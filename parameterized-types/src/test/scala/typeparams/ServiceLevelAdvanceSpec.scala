package typeparams
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServiceLevelAdvanceSpec extends AnyFlatSpec with Matchers {
  "advance" should "upgrade service level" in {
    """
    val test = new ServiceLevelAdvance[Economy].advance[Business]
    """ should compile
  }

  "advance" should "not downgrade service level" in {
    """
    val test = new ServiceLevelAdvance[Elite].advance[Business]
    """ shouldNot typeCheck
  }

  "advance" should "not upgrade from different children" in {
    """
    val test = new ServiceLevelAdvance[Special1b].advance[Elite]
    """ shouldNot typeCheck
  }
}
