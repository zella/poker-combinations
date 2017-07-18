package org.zella.combination

import org.zella.cards.ICard

/**
  * Resolves table and players cards to ordered table of combinations and weights
  *
  * @author zella.
  */
trait ICombinationResolver[T] {

  /**
    * Resolve table and players cards to ordered table of combinations and weights
    *
    * @param playersCards map with player and its cards
    * @param tableCards   cards on table
    * @return ordered seq type of [(combination, combination weight), player]
    */
  def resolve(playersCards: Map[T, Seq[ICard]], tableCards: Seq[ICard]): Seq[(Combination, T)]

}
