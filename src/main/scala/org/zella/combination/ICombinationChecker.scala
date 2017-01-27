package org.zella.combination

import org.zella.cards.ICard

/**
  *
  * Проверяет является - ли набор карт комбинацией
  *
  * возращает карты, содержащие комбинацию и вес комбинации без учета карт юзера
  *
  * @author zella.
  */
trait ICombinationChecker {

  def check1RoyalFlush(): Option[(Seq[ICard], Long)]

  def check2StraightFlush(): Option[(Seq[ICard], Long)]

  def check3Kare(): Option[(Seq[ICard], Long)]

  def check4FullHouse(): Option[(Seq[ICard], Long)]

  def check5Flush(): Option[(Seq[ICard], Long)]

  def check6Straight(): Option[(Seq[ICard], Long)]

  def check7Three(): Option[(Seq[ICard], Long)]

  def check8TwoPair(): Option[(Seq[ICard], Long)]

  def check9Pair(): Option[(Seq[ICard], Long)]

  def check10HighCard(): Option[(Seq[ICard], Long)]

}
