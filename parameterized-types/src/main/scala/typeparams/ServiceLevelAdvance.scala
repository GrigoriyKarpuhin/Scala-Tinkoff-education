package typeparams

class Economy

class UpgradedEconomy extends Economy

class Special1b extends UpgradedEconomy

class ExtendedEconomy extends Economy

class Business extends ExtendedEconomy

class Elite extends Business

class Platinum extends Business

class ServiceLevelAdvance[T <: Economy] {
  def advance[U <: T]: ServiceLevelAdvance[U] = new ServiceLevelAdvance[U]
}
