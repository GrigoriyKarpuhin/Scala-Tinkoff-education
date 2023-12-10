package converter

object Currencies {
  val SupportedCurrencies = Set("RUB", "USD", "EUR")
}

class CurrencyConverter(ratesDictionary: Map[String, Map[String, BigDecimal]]) {

  import Currencies.SupportedCurrencies

  def exchange(money: Money, toCurrency: String): Money = {
    if (!SupportedCurrencies.contains(money.currency) || !SupportedCurrencies.contains(toCurrency))
      throw new UnsupportedCurrencyException
    if (money.currency.equals(toCurrency)) throw new WrongCurrencyException

    val exchangeRate = ratesDictionary(money.currency)(toCurrency)

    val convertedAmount = money.amount * exchangeRate

    Money(convertedAmount, toCurrency)
  }
}

object CurrencyConverter {

  import Currencies.SupportedCurrencies

  def apply(ratesDictionary: Map[String, Map[String, BigDecimal]]) = {
    val fromCurrencies = ratesDictionary.keys
    val toCurrencies = ratesDictionary.values
    if (
      fromCurrencies.toSet
        .subsetOf(SupportedCurrencies) && toCurrencies.forall(_.keys.toSet.subsetOf(SupportedCurrencies))
    )
      new CurrencyConverter(ratesDictionary)
    else throw new UnsupportedCurrencyException
  }
}
