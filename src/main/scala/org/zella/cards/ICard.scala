package org.zella.cards

import com.danko.utils.Json
import com.fasterxml.jackson.databind.JsonNode
import org.zella.cards.Ranks._
import org.zella.cards.Suits._

import scala.collection.mutable

//TODO change modularity

//TODO simplification (Weigth+CombinationWeight=Weight)

/**
  * @author zella.
  */
trait ICard extends Weight {

  def suit: Suit

  def rank: Rank

  def toJson: JsonNode
}

case class Card(_suit: Suit, _rank: Rank) extends ICard {
  override def suit: Suit = _suit

  override def rank: Rank = _rank

  override val getWeight: Int = rank.getWeight

  override def toJson: JsonNode = {
    val json = Json.newObject()
    json.put("r", _rank.id)
    json.put("s", _suit.name)
  }
}


trait Weight extends Ordered[Weight] {
  val getWeight: Int

  override def compare(that: Weight): Int = this.getWeight compareTo that.getWeight
}

object Weight {
  implicit def ordering[T <: Weight]: Ordering[T] = Ordering.by(w => w.getWeight)
}

/**
  * Масть
  */
trait Suit extends Weight {
  def name: String
}

object Suits {

  //пики
  case object Spade extends Suit {
    override val getWeight: Int = 1

    override def name: String = "S"
  }

  //крести(трефы)
  case object Club extends Suit {
    override val getWeight: Int = 2

    override def name: String = "C"
  }

  //буби
  case object Diamond extends Suit {
    override val getWeight: Int = 3

    override def name: String = "D"
  }

  //черви
  case object Heart extends Suit {
    override val getWeight: Int = 4

    override def name: String = "H"
  }

}

trait Rank extends Weight {
  def id: Int
}

object Ranks {

  case object K extends Rank {
    override def id: Int = 13

    override val getWeight: Int = 13
  }

  case object Q extends Rank {
    override def id: Int = 12

    override val getWeight: Int = 12
  }

  case object J extends Rank {
    override def id: Int = 11

    override val getWeight: Int = 11
  }

  case class N(n: Int) extends Rank {
    override def id: Int = n

    override val getWeight: Int = id
  }

  case object A extends Rank {
    override def id: Int = 1

    override val getWeight: Int = 14
  }

}
