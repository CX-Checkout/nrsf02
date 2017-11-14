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

  it should "correctly apply getMoreOffers" in {
    // Given
    val  skus = "AAEABCCE"
    val getMoreOffers = Map(
      'E' -> (2, "B")
    )

    // When
    val result = Checkout.applyGetMoreOffers(skus, getMoreOffers)

    // Then
    result shouldBe "AAEABCCEB"
  }


  it should "correctly apply getMoreOffers with many occurrences" in {
    // Given
    val  skus = "AEAEABEECCE"
    val getMoreOffers = Map(
      'E' -> (2, "B")
    )

    // When
    val result = Checkout.applyGetMoreOffers(skus, getMoreOffers)

    // Then
    result shouldBe "AEAEABEECCEBB"
  }


  it should "correctly apply special offers" in {
    // Given
    val skus = CheckoutStep(List(GroupedSku("A", 4), GroupedSku("B", 3), GroupedSku("C", 2)), 0)

    val specialOffers = Map(
      "A" -> Map(3 -> 130, 5 -> 200),
      "B" -> Map(2 -> 45)
    )

    val prices = Map(
      "A" -> 50,
      "B" -> 30,
      "C" -> 20,
      "D" -> 15
    )

    // When
    val result = Checkout.applySpecialOffers(skus, specialOffers, prices)

    // Then
    result shouldBe 175 + 50 + 30 + 40
  }

  it should "remove sku from the basket if the special offer consumes all the items" in {
    // Given
    val skus = CheckoutStep(List(GroupedSku("A", 3), GroupedSku("B", 2), GroupedSku("C", 2)), 0)

    val specialOffers = Map(
      "A" -> Map(3-> 130, 5-> 200),
      "B" -> Map(2-> 45),
    )

    val prices = Map(
      "A" -> 50,
      "B" -> 30,
      "C" -> 20,
      "D" -> 15
    )

    // When
    val result = Checkout.applySpecialOffers(skus, specialOffers, prices)

    // Then
    result shouldBe 215
  }

  it should "combine offers to get the best result possible" in {
    // Given
    val skus = CheckoutStep(List(GroupedSku("A", 8)), 0)

    val specialOffers = Map(
      "A" -> Map(3-> 130, 5-> 200)
    )

    val prices = Map("A" -> 50)

    // When
    val result = Checkout.applySpecialOffers(skus, specialOffers, prices)

    // Then
    result shouldBe 330
  }

  it should "calculate correct result for 6 A items with special offers" in {
    // Given
    val quantity = 6

    val specialOffers = Map(3-> 130, 5-> 200)

    val fixedPrice = 50

    // When
    val result = Checkout.findBestSpecialOffersCombination(quantity, specialOffers, fixedPrice, 0)

    // Then
    result shouldBe 250
  }

  it should "calculate correct result for 7 A items with special offers" in {
    // Given
    val quantity = 7

    val specialOffers = Map(3-> 130, 5-> 200)

    val fixedPrice = 50

    // When
    val result = Checkout.findBestSpecialOffersCombination(quantity, specialOffers, fixedPrice, 0)

    // Then
    result shouldBe 300
  }

  it should "calculate correct result for 9 A items with special offers" in {
    // Given
    val quantity = 9

    val specialOffers = Map(3-> 130, 5-> 200)

    val fixedPrice = 50

    // When
    val result = Checkout.findBestSpecialOffersCombination(quantity, specialOffers, fixedPrice, 0)

    // Then
    result shouldBe 380
  }

}
