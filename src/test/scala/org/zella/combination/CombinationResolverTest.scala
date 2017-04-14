package org.zella.combination


import org.junit.Test
import org.scalatest.Matchers
import org.zella.cards.Ranks._
import org.zella.cards.Suits._
import org.zella.cards.{Card, _}
import org.zella.combination.impl.CombinationResolver

import scala.collection.mutable

/**
  * @author zella.
  */
class CombinationResolverTest extends Matchers {

  @Test
  def resolve(): Unit = {

    val playersAndCards = Map[String, Seq[ICard]](
      "1" -> Seq(Card(Heart, A), Card(Diamond, K)),
      "2" -> Seq(Card(Heart, N(10)), Card(Diamond, N(10))),
      "3" -> Seq(Card(Club, Q), Card(Diamond, J)),
      "4" -> Seq(Card(Diamond, A), Card(Heart, K)),
      "5" -> Seq(Card(Diamond, N(2)), Card(Spade, N(2)))
    )

    val tableCards = Seq(
      Card(Heart, N(2)),
      Card(Heart, N(3)),
      Card(Spade, N(10)),
      Card(Heart, J),
      Card(Club, N(9)))

    //combinations should be
    //A
    val combPlayer1 = (
      Combination(
        Seq(Card(Heart, A), Card(Diamond, K), Card(Heart, J), Card(Spade, N(10)), Card(Club, N(9))),
        Seq(Card(Heart, A)),
        10000000000L + 14 * 100000000 + 13 * 1000000 + 11 * 10000 + 10 * 100 + 9 * 1
      ),
      "1")
    //10,10,10
    val combPlayer2 = (
      Combination(
        Seq(Card(Heart, N(10)), Card(Diamond, N(10)), Card(Spade, N(10)), Card(Heart, J), Card(Club, N(9))),
        Seq(Card(Heart, N(10)), Card(Diamond, N(10)), Card(Spade, N(10))),
        40000000000L + 10 * 100000000 + 11 * 1000000 + 9 * 10000),
      "2")
    //JJ
    val combPlayer3 = (
      Combination(
        Seq(Card(Diamond, J), Card(Heart, J), Card(Club, Q), Card(Spade, N(10)), Card(Club, N(9))),
        Seq(Card(Diamond, J), Card(Heart, J)),
        20000000000L + 11 * 100000000 + 12 * 1000000 + 10 * 10000 + 9 * 100),
      "3")
    //A
    val combPlayer4 = (
      Combination(
        Seq(Card(Diamond, A), Card(Heart, K), Card(Heart, J), Card(Spade, N(10)), Card(Club, N(9))),
        Seq(Card(Diamond, A)),
        10000000000L + 14 * 100000000 + 13 * 1000000 + 11 * 10000 + 10 * 100 + 9 * 1),
      "4")
    //2,2,2
    val combPlayer5 = (
      Combination(
        Seq(Card(Diamond, N(2)), Card(Spade, N(2)), Card(Heart, N(2)), Card(Heart, J), Card(Spade, N(10))),
        Seq(Card(Diamond, N(2)), Card(Spade, N(2)), Card(Heart, N(2))),
        40000000000L + 2 * 100000000 + 11 * 1000000 + 10 * 10000),
      "5")

    val resolver = new CombinationResolver[String]()

    val computed: Seq[(Combination, String)] = resolver.resolve(playersAndCards, tableCards)

    computed.toIndexedSeq(0) shouldEqual combPlayer2
    computed.toIndexedSeq(1) shouldEqual combPlayer5
    computed.toIndexedSeq(2) shouldEqual combPlayer3
    computed.toIndexedSeq(3) should (equal(combPlayer4) or equal(combPlayer1))
    computed.toIndexedSeq(4) should (equal(combPlayer1) or equal(combPlayer4))

    computed should (
      contain theSameElementsInOrderAs Seq(combPlayer2, combPlayer5, combPlayer3, combPlayer4, combPlayer1)
        or contain theSameElementsInOrderAs Seq(combPlayer2, combPlayer5, combPlayer3, combPlayer1, combPlayer4)
      )


  }


}
