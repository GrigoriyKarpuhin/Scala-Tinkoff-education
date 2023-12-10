package parquet_price

import scala.math.pow

sealed trait HouseType

case object Premium extends HouseType
case object Econom extends HouseType

case class House(hType: HouseType, floorsNumber: Int, length: Int, width: Int, height: Int)

object House {

  def apply(hType: HouseType, floorsNumber: Int, length: Int, width: Int, height: Int): House = {
    if (floorsNumber <= 0 || length <= 0 || width <= 0 || height <= 0) throw new WrongHouseOptionsException
    new House(hType, floorsNumber, length, width, height)
  }

  def parquet(house: House): Int = {
    house match {
      case House(Premium, floorsNumber, _, _, _) if floorsNumber < 5 =>
        pow(3, floorsNumber).toInt * (house.length + house.width + house.height)
      case House(Premium, floorsNumber, _, _, _) if floorsNumber >= 5 =>
        pow(2, floorsNumber).toInt * (house.length + house.width + house.height)
      case House(Econom, _, _, _, _) =>
        house.length * house.width * house.height + house.floorsNumber * 10000
    }
  }
}
