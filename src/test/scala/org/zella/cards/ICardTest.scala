package org.zella.cards

import org.junit.Test
import org.scalatest.Matchers
import play.api.libs.json.Json

/**
  * @author zella.
  */
class ICardTest extends Matchers {
  
  @Test
  def fromJson(): Unit = {
    Card.fromJson(Json.obj("r" -> 2, "s" -> "H")) shouldBe Card(Suits.Heart, Ranks.N(2))
    Card.fromJson(Json.obj("r" -> 10, "s" -> "H")) shouldBe Card(Suits.Heart, Ranks.N(10))
    Card.fromJson(Json.obj("r" -> 1, "s" -> "C")) shouldBe Card(Suits.Club, Ranks.A)
    Card.fromJson(Json.obj("r" -> 11, "s" -> "D")) shouldBe Card(Suits.Diamond, Ranks.J)
    Card.fromJson(Json.obj("r" -> 12, "s" -> "S")) shouldBe Card(Suits.Spade, Ranks.Q)
    Card.fromJson(Json.obj("r" -> 13, "s" -> "S")) shouldBe Card(Suits.Spade, Ranks.K)
  }

  @Test
  def fromJsonInvalidRank(): Unit = {
    assertThrows[MatchError] {
      Card.fromJson(Json.obj("r" -> 0, "s" -> "H"))
    }
    assertThrows[MatchError] {
      Card.fromJson(Json.obj("r" -> 14, "s" -> "H"))
    }
  }

  @Test
  def fromJsonInvalidSuit(): Unit = {
    assertThrows[MatchError] {
      Card.fromJson(Json.obj("r" -> 10, "s" -> "Y"))
    }
  }
}