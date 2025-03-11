class LoyaltyCardLogic {
  def applyForCard(customer: Customer, cardType: LoyaltyCard): Either[String, Customer] = {
    if (customer.loyaltyCard.isDefined) Left("Customer already has a loyalty card.")
    else if (customer.visits < 5 || customer.age < 18) Left("Not eligible for a loyalty card.")
    else Right(customer.copy(loyaltyCard = Some(cardType)))
  }
}

trait LoyaltyCard

case class DrinksLoyaltyCard(stamps: Int = 0) extends LoyaltyCard

case class DiscountLoyaltyCard(stars: Int = 0) extends LoyaltyCard