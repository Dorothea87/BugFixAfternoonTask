import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LoyaltyCardLogicSpec extends AnyWordSpec with Matchers {

  val loyaltyCardLogic = new LoyaltyCardLogic

  "LoyaltyScheme" should {
    "deny loyalty card application if under 18" in {
      val customer = Customer("Alice", age = 17, visits = 6, totalSpend = 200)
      loyaltyCardLogic.applyForCard(customer, DiscountLoyaltyCard()) shouldBe Left("Not eligible for a loyalty card.")
    }
    "deny loyalty card application if loyalty card already exists" in {
      val customer = Customer("Peter", age = 34, visits = 6, totalSpend = 200, loyaltyCard = Some(DiscountLoyaltyCard()))
      loyaltyCardLogic.applyForCard(customer, DiscountLoyaltyCard()) shouldBe Left("Customer already has a loyalty card.")
    }
    "deny loyalty card application if customer has less than 5 visits" in {
      val customer = Customer("Mary", age = 27, visits = 4, totalSpend = 100)
      loyaltyCardLogic.applyForCard(customer, DiscountLoyaltyCard()) shouldBe Left("Not eligible for a loyalty card.")
    }
    "grant discount card if criteria met" in {
      val customer = Customer("Bob", age = 20, visits = 6, totalSpend = 200)
      loyaltyCardLogic.applyForCard(customer, DiscountLoyaltyCard()) shouldBe Right(customer.copy(loyaltyCard = Some(DiscountLoyaltyCard())))
    }
    "grant discount card if customer is exactly 18" in {
      val customer = Customer("Barbara", age = 18, visits = 7, totalSpend = 150)
      loyaltyCardLogic.applyForCard(customer, DiscountLoyaltyCard()) shouldBe Right(customer.copy(loyaltyCard = Some(DiscountLoyaltyCard())))
    }
    "deny drinks loyalty card application if under 18" in {
      val customer = Customer("Frank", age = 17, visits = 6, totalSpend = 200)
      loyaltyCardLogic.applyForCard(customer, DrinksLoyaltyCard()) shouldBe Left("Not eligible for a loyalty card.")
    }
    "deny drinks loyalty card application if drinks loyalty card already exists" in {
      val customer = Customer("John", age = 34, visits = 6, totalSpend = 200, loyaltyCard = Some(DrinksLoyaltyCard()))
      loyaltyCardLogic.applyForCard(customer, DrinksLoyaltyCard()) shouldBe Left("Customer already has a loyalty card.")
    }
    "grant drinks discount card if criteria met" in {
      val customer = Customer("Lucy", age = 20, visits = 6, totalSpend = 200)
      loyaltyCardLogic.applyForCard(customer, DrinksLoyaltyCard()) shouldBe Right(customer.copy(loyaltyCard = Some(DrinksLoyaltyCard())))
    }
    "grant drinks discount card if customer is exactly 18" in {
      val customer = Customer("Betty", age = 18, visits = 7, totalSpend = 150)
      loyaltyCardLogic.applyForCard(customer, DrinksLoyaltyCard()) shouldBe Right(customer.copy(loyaltyCard = Some(DrinksLoyaltyCard())))
    }
    "deny drinks loyalty card application if customer has less than 5 visits" in {
      val customer = Customer("Jules", age = 27, visits = 4, totalSpend = 100)
      loyaltyCardLogic.applyForCard(customer, DrinksLoyaltyCard()) shouldBe Left("Not eligible for a loyalty card.")
    }
  }
}
