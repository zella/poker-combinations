package org.zella.cards

import org.zella.cards.Ranks._
import org.zella.cards.Suits._
import play.api.libs.json.{JsValue, Json}

/**
  * @author zella.
  */


trait Weight extends Ordered[Weight] {
  val weight: Int

  override def compare(that: Weight): Int = this.weight compareTo that.weight
}

object Weight {
  implicit def ordering[T <: Weight]: Ordering[T] = Ordering.by(w => w.weight)
}


trait ICard extends Weight {

  def suit: Suit

  def rank: Rank

  def toJson: JsValue
}

case class Card(_suit: Suit, _rank: Rank) extends ICard {
  override def suit: Suit = _suit

  override def rank: Rank = _rank

  override val weight: Int = rank.weight

  override lazy val toJson: JsValue = {
    Json.obj(
      "r" -> _rank.id,
      "s" -> _suit.name
    )
  }
}

object Card {
  def fromJson(json: JsValue): Card = {
    Card(
      json.\("s").as[String] match {
        case "S" => Spade
        case "C" => Club
        case "D" => Diamond
        case "H" => Heart
      },
      json.\("r").as[Int] match {
        case 1 => A
        case 13 => K
        case 12 => Q
        case 11 => J
        case n if n >= 2 && n <= 10 => N(n)
      }
    )
  }
}


/**
  * Масть
  */

object Suits {

  trait Suit extends Weight {
    def name: String
  }

  //пики
  case object Spade extends Suit {
    override val weight: Int = 1

    override def name: String = "S"
  }

  //крести(трефы)
  case object Club extends Suit {
    override val weight: Int = 2

    override def name: String = "C"
  }

  //буби
  case object Diamond extends Suit {
    override val weight: Int = 3

    override def name: String = "D"
  }

  //черви
  case object Heart extends Suit {
    override val weight: Int = 4

    override def name: String = "H"
  }

}


object Ranks {

  trait Rank extends Weight {
    def id: Int
  }

  case object K extends Rank {
    override def id: Int = 13

    override val weight: Int = 13
  }

  case object Q extends Rank {
    override def id: Int = 12

    override val weight: Int = 12
  }

  case object J extends Rank {
    override def id: Int = 11

    override val weight: Int = 11
  }

  case class N(n: Int) extends Rank {
    override def id: Int = n

    override val weight: Int = id
  }

  case object A extends Rank {
    override def id: Int = 1

    override val weight: Int = 14
  }

}
