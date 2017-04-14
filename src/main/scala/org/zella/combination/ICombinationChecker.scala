package org.zella.combination

/**
  *
  * Проверяет является - ли набор карт комбинацией
  *
  * возращает карты, содержащие комбинацию и вес комбинации без учета карт юзера
  *
  * @author zella.
  */
trait ICombinationChecker {

  def check1RoyalFlush(): Option[Combination]

  def check2StraightFlush(): Option[Combination]

  def check3Kare(): Option[Combination]

  def check4FullHouse(): Option[Combination]

  def check5Flush(): Option[Combination]

  def check6Straight(): Option[Combination]

  def check7Three(): Option[Combination]

  def check8TwoPair(): Option[Combination]

  def check9Pair(): Option[Combination]

  def check10HighCard(): Option[Combination]

}
