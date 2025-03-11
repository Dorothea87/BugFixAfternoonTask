import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class CafeLogicSpec extends AnyWordSpec with Matchers {

  val cafeLogic = new CafeLogic

  //Menu Items
  val coffee: StandardItem = StandardItem("Coffee", 3.25, HotDrink)
  val icedTea: StandardItem = StandardItem("Iced Tea", 2.95, ColdDrink)
  val sandwich: StandardItem = StandardItem("Sandwich", 5.95, ColdFood)
  val toastie: StandardItem = StandardItem("Toastie", 7.95, HotFood)
  val expensiveItem: StandardItem = StandardItem("Expensive Item", 100.00, HotFood)
  val burger: PremiumSpecial = PremiumSpecial("Wagyu Burger", 15.00)
  val expensivePremiumItem: StandardItem = StandardItem("Expensive Premium Item", 100.00, HotFood)

  //Current stock
  val stock: StockLevel = StockLevel(Map(
    coffee -> 10,
    icedTea -> 3,
    sandwich -> 17,
    toastie -> 20,
    expensiveItem -> 2,
    burger -> 5,
    expensivePremiumItem -> 1
  ))

  //Order
  val justDrinkOrder: Order = Order(List(coffee))
  val drinkAndColdFoodOrder: Order = Order(List(coffee, sandwich))
  val drinkAndHotFoodOrder: Order = Order(List(coffee, toastie))
  val drinkAndPremiumItemOrder: Order = Order(List(coffee, burger))
  val veryExpensiveOrder: Order = Order(List(expensivePremiumItem))

  //Customers
  val discountCardCustomer: Customer = Customer(
    "Eve",
    age = 25,
    visits = 10,
    totalSpend = 300,
    loyaltyCard = Some(DiscountLoyaltyCard(5))
  )


  "place order" should {
    "place an order if stock is available and reduce the stock count" in {
      val result = cafeLogic.placeOrder(stock, drinkAndColdFoodOrder)
      val expectedResult = Right(StockLevel(Map(
        coffee -> 9,
        icedTea -> 3,
        sandwich -> 16,
        toastie -> 20,
        expensiveItem -> 2,
        burger -> 5,
        expensivePremiumItem -> 1
      )))
      result shouldBe expectedResult
    }
    "fail to place an order if stock is zero" in {
      val noBurger = StockLevel(Map(
        coffee -> 10,
        icedTea -> 3,
        sandwich -> 17,
        toastie -> 20,
        expensiveItem -> 2,
        burger -> 0,
        expensivePremiumItem -> 1
      ))

      val result = cafeLogic.placeOrder(noBurger, drinkAndPremiumItemOrder)
      result shouldBe Left("Item 'Wagyu Burger' is out of stock.")
    }
  }

  "calculateServiceCharge" should {
    "apply no charge for drinks only" in {
      cafeLogic.calculateServiceCharge(justDrinkOrder) shouldBe BigDecimal(0.00)
    }
    "apply 10% for an order containing cold food" in {
      cafeLogic.calculateServiceCharge(drinkAndColdFoodOrder) shouldBe BigDecimal(0.92)
    }
    "apply 20% service charge for hot food" in {
      cafeLogic.calculateServiceCharge(drinkAndHotFoodOrder) shouldBe BigDecimal(2.24)
    }
    "apply 25% service charge for premium meal" in {
      cafeLogic.calculateServiceCharge(drinkAndPremiumItemOrder) shouldBe BigDecimal(4.56)
    }
  }

  "generateBill" should {
    "apply discount and generate correct bill" in {

      //Current working:
      //drink(3.25) + burger(15.00) = 18.25
      //(18.25 - discount(1.86)) + service charge(4.56) = 20.95

      //BUG FOUND!!!! It doesn't appear to be subtracting the discount... My testing isn't helping me identify the problem currently...
      cafeLogic.generateBill(veryExpensiveOrder, discountCardCustomer) shouldBe BigDecimal(110.00)
      // What should be happening...?
      // 10% discount should be applied on the order before service charge. Service charge should take the full order total.
    }
  }
}
