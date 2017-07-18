package org.zella.combination

import org.zella.cards.ICard
import org.zella.combination.CombinationType.CombinationType
import org.zella.combination.impl.CombinationChecker._
import play.api.libs.json.{JsValue, Json}

/**
  *
  * @param cards       5 карт
  * @param combination карты из которой состоит комбинация, например AAA при тройке
  * @param weight      вес комбинации
  */
case class Combination(cards: Seq[ICard], combination: Seq[ICard], weight: Long) {

  lazy val combinationIndexed: IndexedSeq[ICard] = combination.toIndexedSeq

  lazy val combinationType: CombinationType = {
    weight match {
      case x if WEIGHT_HIGHCARD <= x && x < WEIGHT_ONE_PAIR => CombinationType.HIGHCARD
      case x if WEIGHT_ONE_PAIR <= x && x < WEIGHT_TWO_PAIR => CombinationType.ONE_PAIR
      case x if WEIGHT_TWO_PAIR <= x && x < WEIGHT_THREE => CombinationType.TWO_PAIR
      case x if WEIGHT_THREE <= x && x < WEIGHT_STRAIGHT => CombinationType.THREE
      case x if WEIGHT_STRAIGHT <= x && x < WEIGHT_FLUSH => CombinationType.STRAIGHT
      case x if WEIGHT_FLUSH <= x && x < WEIGHT_FULL_HOUSE => CombinationType.FLUSH
      case x if WEIGHT_FULL_HOUSE <= x && x < WEIGHT_KARE => CombinationType.FULL_HOUSE
      case x if WEIGHT_KARE <= x && x < WEIGHT_STRAIGHTFLUSH => CombinationType.KARE
      case x if WEIGHT_STRAIGHTFLUSH <= x && x < WEIGHT_ROYALFLUSH => CombinationType.STRAIGHTFLUSH
      case x if x >= WEIGHT_ROYALFLUSH => CombinationType.ROYALFLUSH
    }
  }

  def toJson: JsValue = {
    Json.obj("cards" -> cards.map(_.toJson),
      "combination" -> combination.map(_.toJson),
      "weight" -> weight
    )
  }

}

object CombinationType extends Enumeration {
  type CombinationType = Value
  val HIGHCARD, ONE_PAIR, TWO_PAIR, THREE, STRAIGHT, FLUSH, FULL_HOUSE, KARE, STRAIGHTFLUSH, ROYALFLUSH = Value
}