package parquet_price

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HouseSpec extends AnyFlatSpec with Matchers {

  "house" should "throw WrongHouseOptionsException if try to crate house with wrong arguments" in {
    assertThrows[WrongHouseOptionsException] {
      House(Econom, 0, 4, 5, 1)
      House(Econom, 1, -4, 5, 1)
      House(Econom, 1, 4, -8, -7)
    }
  }

  "parquet" should "correctly count the price" in {
    val test1 = House(Premium, 2, 4, 4, 4)
    val test2 = House(Premium, 6, 3, 2, 1)
    val test3 = House(Econom, 5, 3, 3, 3)

    House.parquet(test1) shouldEqual 108
    House.parquet(test2) shouldEqual 384
    House.parquet(test3) shouldEqual 50027
    // Как-то неэкономно получилось )))
  }
}
