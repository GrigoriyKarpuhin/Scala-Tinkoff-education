package converter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CurrencyConverterSpec extends AnyFlatSpec with Matchers {
  "exchange" should "convert money for supported currencies" in {
    val rates = Map(
      "USD" -> Map("RUB" -> BigDecimal(72.5)),
      "RUB" -> Map("USD" -> BigDecimal(1 / 72.5))
    )
    val converter = CurrencyConverter(rates)
    val exchangedRub = converter.exchange(Money(2, "USD"), "RUB")
    val exchangedUsd = converter.exchange(Money(10, "RUB"), "USD")
    exchangedRub.amount shouldEqual 145
    exchangedRub.currency shouldEqual "RUB"
    exchangedUsd.amount shouldEqual BigDecimal(1 / 7.25)
    exchangedUsd.currency shouldEqual "USD"
  }

  "converted constructor" should "throw UnsupportedCurrencyException if rates dictionary contains wrong currency" in {
    val rates = Map(
      "GBP" -> Map("RUB" -> BigDecimal(85))
    )
    assertThrows[UnsupportedCurrencyException] {
      CurrencyConverter(rates)
    }
  }

  "-" should "throw MoneyAmountShouldBePositiveException if result is negative" in {
    val first = Money(54, "RUB")
    val second = Money(228, "RUB")
    assertThrows[MoneyAmountShouldBePositiveException] {
      first - second
    }
  }

  "-" should "throw WrongCurrencyException if try to perform an operation between different currencies" in {
    val first = Money(54, "RUB")
    val second = Money(228, "EUR")
    assertThrows[WrongCurrencyException] {
      first - second
    }
  }

  "+" should "throw WrongCurrencyException if try to perform an operation between different currencies" in {
    val first = Money(54, "USD")
    val second = Money(228, "EUR")
    assertThrows[WrongCurrencyException] {
      first + second
    }
  }

  "exchange" should "throw WrongCurrencyException if try to exchange same currency" in {
    val rates = Map(
      "USD" -> Map("RUB" -> BigDecimal(72.5)),
      "RUB" -> Map("USD" -> BigDecimal(1 / 72.5))
    )
    val converter = CurrencyConverter(rates)
    assertThrows[WrongCurrencyException] {
      converter.exchange(Money(2, "USD"), "USD")
    }
  }

  "exchange" should "throw UnsupportedCurrencyException if try to exchange invalid currency" in {
    val rates = Map(
      "USD" -> Map("RUB" -> BigDecimal(72.5)),
      "RUB" -> Map("USD" -> BigDecimal(1 / 72.5))
    )
    val converter = CurrencyConverter(rates)
    assertThrows[UnsupportedCurrencyException] {
      converter.exchange(Money(2, "USD"), "BBC")
    }
  }

  "Money" should "MoneyAmountShouldBePositiveException if try to create negative amount" in {
    assertThrows[MoneyAmountShouldBePositiveException] {
      val test = Money (-23, "RUB")
    }
  }
}
