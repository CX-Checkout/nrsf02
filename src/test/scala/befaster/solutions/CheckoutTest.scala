package befaster.solutions

import befaster.solutions.Checkout.{CheckoutStep, GroupedSku}
import org.scalatest.{FlatSpec, Matchers}

class CheckoutTest extends FlatSpec with Matchers {

  it should "return -1 if skus contain non-valid characters" in {
    // Given
    val  skus = "a-b,s"

    // When
    val result = Checkout.checkout(skus)

    // Then
    result shouldBe -1
  }

  it should "correctly group all the SKUs" in {
    // Given
    val  skus = "AAABCC"

    // When
    val result = Checkout.groupSku(skus)

    // Then
    result should contain theSameElementsAs List(GroupedSku("A", 3), GroupedSku("B", 1), GroupedSku("C", 2))
  }


  it should "correctly apply special offers" in {
    // Given
    val skus = CheckoutStep(List(GroupedSku("A", 4), GroupedSku("B", 3), GroupedSku("C", 2)), 0)

    val specialOffers = Map(
      "A" -> (3, 130),
      "B" -> (2, 45),
    )

    // When
    val result = Checkout.applySpecialOffers(skus, specialOffers)

    // Then
    result.groupedSku should contain theSameElementsAs List(GroupedSku("A", 1), GroupedSku("B", 1), GroupedSku("C", 2))
    result.price shouldBe 175
  }

  it should "remove sku from the basket if the special offer consumes all the items" in {
    // Given
    val skus = CheckoutStep(List(GroupedSku("A", 3), GroupedSku("B", 2), GroupedSku("C", 2)), 0)

    val specialOffers = Map(
      "A" -> (3, 130),
      "B" -> (2, 45),
    )

    // When
    val result = Checkout.applySpecialOffers(skus, specialOffers)

    // Then
    result shouldBe CheckoutStep(List(GroupedSku("C", 2)), 175)
  }

  it should "correcty apply remaining prices to the items" in {
    // Given
    val skus = CheckoutStep(List(GroupedSku("A", 3), GroupedSku("B", 2), GroupedSku("C", 2)), 100)

    val prices = Map(
      "A" -> 50,
      "B" -> 30,
      "C" -> 20,
      "D" -> 15
    )

    // When
    val result = Checkout.applyPrices(skus, prices)

    // Then
    result shouldBe (150 + 60 + 40 + 100)
  }
}
