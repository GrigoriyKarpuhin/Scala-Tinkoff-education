package converter

case class Money private (amount: BigDecimal, currency: String) {
  def +(other: Money): Money = {
    if (!isSameCurrency(other)) throw new WrongCurrencyException
    Money(amount + other.amount, currency)
  }

  def -(other: Money): Money = {
    if (!isSameCurrency(other)) throw new WrongCurrencyException
    if (amount < other.amount)
      throw new MoneyAmountShouldBePositiveException
    Money(amount - other.amount, currency)
  }

  def isSameCurrency(other: Money): Boolean = {
    if (currency == other.currency) true
    else false
  }
}

object Money {

  import Currencies.SupportedCurrencies

  def apply(amount: BigDecimal, currency: String): Money = {
    if (amount < 0) throw new MoneyAmountShouldBePositiveException
    if (!SupportedCurrencies.contains(currency))
      throw new UnsupportedCurrencyException
    new Money(amount, currency)
  }
}
