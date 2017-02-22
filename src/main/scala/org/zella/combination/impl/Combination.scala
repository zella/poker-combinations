package org.zella.combination.impl

import org.zella.cards.ICard

/**
  *
  * @param cards       5 карт
  * @param combination карты из которой состоит комбинация, например AAA при тройке
  * @param weight      вес комбинации
  */
case class Combination(cards: Seq[ICard], combination: Seq[ICard], weight: Long)
