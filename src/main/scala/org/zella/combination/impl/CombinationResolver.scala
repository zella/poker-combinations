package org.zella.combination.impl

import org.zella.cards.ICard
import org.zella.combination.ICombinationResolver

import scala.collection.mutable

/**
  * @author zella.
  */
class CombinationResolver[T] extends ICombinationResolver[T] {

  override def resolve(
                        playersCards: mutable.Map[T, Seq[ICard]],
                        tableCards: Seq[ICard]):
  Seq[((Seq[ICard], Int), T)] = {

    playersCards.map((kv) => resolverSingle(kv._2, tableCards, kv._1)).toSeq.sortBy(tuple => tuple._1._2)
  }

  private def resolverSingle(playerCards: Seq[ICard],
                             tableCards: Seq[ICard],
                             player: T):
  ((Seq[ICard], Int), T) = {

    val cc = new CombinationChecker(playerCards ++ tableCards)

    val combAndWeight = cc.check1RoyalFlush()
      .getOrElse(cc.check2StraightFlush()
        .getOrElse(cc.check3Kare()
          .getOrElse(cc.check4FullHouse()
            .getOrElse(cc.check5Flush()
              .getOrElse(cc.check6Straight()
                .getOrElse(cc.check7Three()
                  .getOrElse(cc.check8TwoPair()
                    .getOrElse(cc.check9Pair()
                      .getOrElse(cc.check10HighCard())))))))))

    combAndWeight match {
      case Some((cards: Seq[ICard], weignt: Int)) => ((cards, weignt), player)
      case None => throw new RuntimeException("Poker combination not found, this should never happen")
    }
  }

}
