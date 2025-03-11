import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StockLevelSpec extends AnyWordSpec with Matchers {
  val coffee: PremiumSpecial = PremiumSpecial("Luxury Coffee", 5.00)
  val burger: PremiumSpecial = PremiumSpecial("Wagyu Burger", 15.00)
  val pasta: PremiumSpecial = PremiumSpecial("Truffle Pasta", 25.00)
  val stock: StockLevel = StockLevel(Map(coffee -> 10, burger -> 5))


  "StockLevel" should {
    "return correct stock count" in {
      stock.getStockCount(coffee) shouldBe 10
      stock.getStockCount(burger) shouldBe 5
      stock.getStockCount(pasta) shouldBe 0 // Not in stock
    }

    "update stock count correctly" in {
      val updatedStock = stock.updateStock(coffee, 8)
      updatedStock.getStockCount(coffee) shouldBe 8
      updatedStock.getStockCount(burger) shouldBe 5
    }

    "handle updating stock to zero" in {
      val updatedStock = stock.updateStock(burger, 0)
      updatedStock.getStockCount(burger) shouldBe 0
      updatedStock.getStockCount(coffee) shouldBe 10 // Coffee should remain unchanged
    }

    "add new items to stock" in {
      val updatedStock = stock.updateStock(pasta, 12)
      updatedStock.getStockCount(pasta) shouldBe 12
      updatedStock.getStockCount(coffee) shouldBe 10 // Coffee remains the same
    }
  }

}
