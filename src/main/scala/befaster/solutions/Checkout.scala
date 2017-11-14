package befaster.solutions

object Checkout {

  private val prices = Map(
    "A" -> 50,
    "B" -> 30,
    "C" -> 20,
    "D" -> 15,
    "E" -> 40
  )

  private val specialOffers = Map(
    "A" -> Map(3 -> 130, 5 -> 200),
    "B" -> Map(2 -> 45)
  )

  private val getMoreOffers = Map(
    'E' -> (20, "B")
  )

  case class GroupedSku(sku: String, quantity: Int)
  case class CheckoutStep(groupedSku: Iterable[GroupedSku], price: Int)

  def checkout(skus: String): Integer = {

    val validItemsSet = prices.keys.mkString("").toSet

    if (skus.toSet.diff(validItemsSet).size > 0) -1
    else {
      val completeSkus = applyGetMoreOffers(skus, getMoreOffers)

      val initialCheckoutStep = CheckoutStep(groupSku(completeSkus), 0)

      val finalPrice = applySpecialOffers(initialCheckoutStep, specialOffers, prices)

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


  def applyGetMoreOffers(skus: String, getMoreOffers: Map[Char, (Int, String)]): String = {
    val appendOffers = getMoreOffers.map {
      case (sku, (quantity, whatToAdd)) =>
        val occurances = skus.count(_ == sku)
        whatToAdd * (occurances / quantity)
    }.mkString("")
    skus + appendOffers
  }

  /**
    * Applies special offers to the current basket, updating the price and the items
    * @param previousStep previous basket
    * @param specialOffers special offer map (we will use the best offer from the offers list)
    * @return current basket after special offer application
    */
  def applySpecialOffers(previousStep: CheckoutStep, specialOffers: Map[String, Map[Int, Int]], prices: Map[String, Int]): Int = {
    val stepWithSpecialOffers = previousStep.groupedSku.map{
      case GroupedSku(sku, quantity) =>

        val skuPrice = prices.getOrElse(sku, 0)

        specialOffers.get(sku) match {

          case Some(offers) => findBestSpecialOffersCombination(quantity, offers, skuPrice, 0)

          case None => skuPrice * quantity
        }

    }

    stepWithSpecialOffers.sum

  }


  /**
    * Recursive (non tail-recursive) function to find out the best combination of offers for the customer
    * @param quantity initial item quantity
    * @param offers offers for this item
    * @param currentPrice price storage
    * @return best price using the offer
    */
  def findBestSpecialOffersCombination(quantity: Int, specialOffersForCurrentSku: Map[Int, Int], fixedPrice: Int, currentPrice: Int): Int = {
    val offersQuantities = specialOffersForCurrentSku.keys

    if (offersQuantities.forall(_ > quantity)) currentPrice + quantity * fixedPrice
    else {
      specialOffersForCurrentSku.collect {
        case (offerQuantity, offerPrice) if quantity >= offerQuantity =>
          findBestSpecialOffersCombination(
            quantity - offerQuantity,
            specialOffersForCurrentSku,
            fixedPrice,
            currentPrice + offerPrice
          )
      }
    }.min
  }

}
