package org.zella.combination.impl

import org.zella.cards.ICard
import org.zella.combination.{Combination, ICombinationResolver}

/**
  * @author zella.
  */
class CombinationResolver[T] extends ICombinationResolver[T] {

  override def resolve(playersCards: Map[T, Seq[ICard]],
                       tableCards: Seq[ICard]): Seq[(T, Combination)] = {

    playersCards.map((kv) => resolverSingle(kv._2, tableCards, kv._1)).toSeq.sortBy(tuple => tuple._2.weight).reverse

  }


  private def checkCombination(cards: Seq[ICard]): Combination = {

    val cc = new CombinationChecker(cards)

    val check1: Option[Combination] = cc.check1RoyalFlush()
    if (check1.isDefined) return check1.get
    val check2: Option[Combination] = cc.check2StraightFlush()
    if (check2.isDefined) return check2.get
    val check3: Option[Combination] = cc.check3Kare()
    if (check3.isDefined) return check3.get
    val check4: Option[Combination] = cc.check4FullHouse()
    if (check4.isDefined) return check4.get
    val check5: Option[Combination] = cc.check5Flush()
    if (check5.isDefined) return check5.get
    val check6: Option[Combination] = cc.check6Straight()
    if (check6.isDefined) return check6.get
    val check7: Option[Combination] = cc.check7Three()
    if (check7.isDefined) return check7.get
    val check8: Option[Combination] = cc.check8TwoPair()
    if (check8.isDefined) return check8.get
    val check9: Option[Combination] = cc.check9Pair()
    if (check9.isDefined) return check9.get
    val check10: Option[Combination] = cc.check10HighCard()
    if (check10.isDefined) return check10.get
    throw new RuntimeException("Poker combination not found, this should never happen, this is bug")
  }

  private def resolverSingle(playerCards: Seq[ICard],
                             tableCards: Seq[ICard],
                             player: T): (T, Combination) = (player, checkCombination(playerCards ++ tableCards))

}

object CombinationResolver {
  def apply[T]: CombinationResolver[T] = new CombinationResolver[T]()
}
