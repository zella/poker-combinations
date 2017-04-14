package org.zella.combination

import org.zella.cards.ICard
import play.api.libs.json.{JsValue, Json}

/**
  *
  * @param cards       5 карт
  * @param combination карты из которой состоит комбинация, например AAA при тройке
  * @param weight      вес комбинации
  */
case class Combination(cards: Seq[ICard], combination: Seq[ICard], weight: Long) {

  def toJson: JsValue = {
    Json.obj("cards" -> cards.map(_.toJson),
      "combination" -> combination.map(_.toJson),
      "weight" -> weight
    )
  }

}
