package building

/** Здание должно иметь:
  *   - строковый адрес
  *   - этажи (сходящиеся к первому этажу) Этаж может быть жилым, коммерческим, либо чердаком (который сам может быть
  *     коммерческим). На каждом жилом этаже живет 2 человека и есть лестница(ссылка) ведущая на следующий этаж У
  *     каждого человека есть возраст (>0) и пол На коммерческом этаже может быть несколько заведений (используйте
  *     Array), но не меньше 1. Здание всегда должно заканчиваться чердаком На чердаке никто не живет, но это может быть
  *     и коммерческое помещение (но только 1).
  */

case class Building(address: String, floors: List[Floor], attic: Attic)

sealed trait Floor {
  val floorNumber: Int
}

case class ResidentialFloor(floorNumber: Int, person1: Tenant, person2: Tenant) extends Floor
case class CommercialFloor(floorNumber: Int, array: Array[String]) extends Floor
case class Tenant(age: Int, gender: String)
sealed trait Attic extends Floor
case class CommercialAttic(floorNumber: Int, room: String) extends Attic {}

object Building {

  /** Проходится по зданию снизу в вверх, применяя функцию [[f]] на каждом жилом этаже с начальным аккумулятором
    * [[accumulator]]
    */
  def fold(building: Building, accumulator: Int)(f: (Int, Floor) => Int): Int = {
    building.floors.foldLeft(accumulator) { (acc, floor) =>
      f(acc, floor)
    }
  }

  /** Подсчитывает количество этаже, на которых живет хотя бы один мужчина старше [[olderThan]]. Используйте [[fold]]
    */
  def countOldManFloors(building: Building, olderThan: Int): Int = {
    def countOnFloor(acc: Int, floor: Floor): Int = {
      floor match {
        case ResidentialFloor(_, person1, person2) =>
          if (
            (person1.gender
              .equals("male") && person1.age > olderThan) && (person2.gender.equals("male") && person2.age > olderThan)
          ) acc + 2
          else if (
            (person1.gender
              .equals("male") && person1.age > olderThan) || (person2.gender.equals("male") && person2.age > olderThan)
          ) acc + 1
          else acc
      }
    }

    fold(building, 0)(countOnFloor)
  }

  /** Находит наибольший возраст женьщины, проживающей в здании. Используйте [[fold]] */
  def womanMaxAge(building: Building): Int = {
    def findMaxAgeOnFloor(acc: Int, floor: Floor): Int = {
      floor match {
        case ResidentialFloor(_, person1, person2) if person1.gender == "female" || person2.gender == "female" =>
          val maxAgeOnFloor =
            List(person1.age, person2.age).filter(_ => person1.gender == "female" || person2.gender == "female").max
          if (maxAgeOnFloor > acc) maxAgeOnFloor else acc
        case _ => acc
      }
    }

    fold(building, 0)(findMaxAgeOnFloor)
  }

  /** Находит кол-во коммерческих заведений в здании. Используйте [[fold]] */
  def countCommercial(building: Building): Int = {
    def countCommercialFloors(acc: Int, floor: Floor): Int = {
      floor match {
        case CommercialFloor(_, array) => acc + array.length
        case CommercialAttic(_, _)     => acc + 1
        case _                         => acc
      }
    }

    fold(building, 0)(countCommercialFloors)
  }

  /* Находит среднее кол-во коммерческих заведений зданиях. Реализуйте свою функцию, похожую на [[fold]] для прохода по зданию */
  def countCommercialAvg(building: Array[Building]): Double = {
    val totalCommercialFloors = building.map(countCommercial).sum
    val totalBuildings = building.length
    if (totalBuildings > 0) totalCommercialFloors.toDouble / totalBuildings.toDouble
    else 0.0
  }

  /* Находит среднее кол-во мужчин на четных этажах. Реализуйте свою функцию, похожую на [[fold]] для прохода по зданию */
  def evenFloorsMenAvg(building: Building): Double = {
    def countMenOnEvenFloors(acc: (Int, Int), floor: Floor): (Int, Int) = {
      floor match {
        case ResidentialFloor(floorNumber, person1, person2) if floorNumber % 2 == 0 =>
          val menCount = (if (person1.gender == "male") 1 else 0) + (if (person2.gender == "male") 1 else 0)
          (acc._1 + menCount, acc._2 + 1)
        case _ => acc
      }
    }
    0.0
  }
}
