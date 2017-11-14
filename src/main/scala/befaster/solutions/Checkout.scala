package befaster.solutions

object Checkout {

  private val prices = Map(
    "A" -> 50,
    "B" -> 30,
    "C" -> 20,
    "D" -> 15,
  )

  private val specialOffers = Map(
    "A" -> (3, 130),
    "B" -> (2, 45)
  )

  case class GroupedSku(sku: String, quantity: Int)
  case class CheckoutStep(groupedSku: Iterable[GroupedSku], price: Int)

  def checkout(skus: String): Integer = {
    val validItemsSet = prices.keys.mkString("").toSet
    if (skus.toSet.diff(validItemsSet).size > 0) -1
    else {

      val initialCheckoutStep = CheckoutStep(groupSku(skus), 0)

      val specialOffersCheckoutStep = applySpecialOffers(initialCheckoutStep, specialOffers)

      val finalPrice = applyPrices(specialOffersCheckoutStep, prices)

      finalPrice
    }
  }

  /**
    * Groups SKUs in a TODO `GroupedSku`
    * @param skus initial input with all the SKU in the basket
    * @return an Iterable of Grouped SKU
    */
  def groupSku(skus: String): Iterable[GroupedSku] = {
    val grouped = skus.groupBy(identity)
    grouped.map {
      case (sku, group) => GroupedSku(sku.toString, group.length)
    }.toList
  }

  /**
    * Applies special offers to the current basket, updating the price and the items
    * @param previousStep previous basket
    * @param specialOffers special offer map
    * @return current basket after special offer application
    */
  def applySpecialOffers(previousStep: CheckoutStep, specialOffers: Map[String, (Int, Int)]): CheckoutStep = {
    val stepWithSpecialOffers = previousStep.groupedSku.map{
      case GroupedSku(sku, quantity) =>
        specialOffers.get(sku) match {
          case Some((offerQuantity, offerPrice)) => (GroupedSku(sku, quantity % offerQuantity), (quantity / offerQuantity) * offerPrice)
          case None => (GroupedSku(sku, quantity), 0)
        }
    }

    val remainingSku = stepWithSpecialOffers.collect {
      case (groupedSku, _) if groupedSku.quantity > 0 => groupedSku
    }

    val stepPrice = stepWithSpecialOffers.map{
      case (_, price) => price
    }.sum

    CheckoutStep(remainingSku, stepPrice)
  }

  /**
    * Applies normal prices for the remaining elements in the basket
    * @param previousStep previous basket
    * @param prices prices map
    * @return prices for the items
    */
  def applyPrices(previousStep: CheckoutStep, prices: Map[String, Int]): Int = {
    val remainingPrice = previousStep.groupedSku.map {
      case GroupedSku(sku, quantity) => prices.getOrElse(sku, 0) * quantity
    }.sum

    previousStep.price + remainingPrice
  }
}
