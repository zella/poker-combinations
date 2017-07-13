package org.zella.combination

import org.junit.Test
import org.scalatest.Matchers
import org.zella.cards.Card
import org.zella.cards.Ranks._
import org.zella.cards.Suits._
import org.zella.combination.impl.CombinationChecker
import org.zella.cards.Card
import org.zella.cards.Ranks._
import org.zella.cards.Suits._

/**
  * @author zella.
  */
class CombinationCheckerTest extends Matchers {


  @Test
  def sortByRank(): Unit = {
    val cards = Seq(
      Card(Club, A),
      Card(Heart, N(10)),
      Card(Club, K),
      Card(Club, N(10)),
      Card(Diamond, N(8)))

    new CombinationChecker(cards).sortedByRank.toSeq should contain theSameElementsInOrderAs Seq(
      Card(Club, A),
      Card(Club, K),
      Card(Heart, N(10)),
      Card(Club, N(10)),
      Card(Diamond, N(8))
    )
  }

  @Test
  def sortByRankTyzLower(): Unit = {
    val cards = Seq(
      Card(Club, A),
      Card(Heart, N(10)),
      Card(Club, K),
      Card(Club, N(10)),
      Card(Diamond, N(8)))

    new CombinationChecker(cards).sortByRankTyzLower(cards) should contain theSameElementsInOrderAs Seq(
      Card(Club, K),
      Card(Heart, N(10)),
      Card(Club, N(10)),
      Card(Diamond, N(8)),
      Card(Club, A)
    )
  }

  @Test
  def royalFlush(): Unit = {

    val cards = Seq(
      Card(Diamond, A),
      Card(Diamond, N(10)),
      Card(Diamond, K),
      Card(Diamond, Q),
      Card(Diamond, J),
      Card(Diamond, N(10)),
      Card(Diamond, N(8))
    )

    new CombinationChecker(cards).check1RoyalFlush() shouldBe Some(Combination(
      Seq(
        Card(Diamond, A),
        Card(Diamond, K),
        Card(Diamond, Q),
        Card(Diamond, J),
        Card(Diamond, N(10))),
      Seq(
        Card(Diamond, A),
        Card(Diamond, K),
        Card(Diamond, Q),
        Card(Diamond, J),
        Card(Diamond, N(10))),
      100000000000L))
  }

  @Test
  def royalFlushNoByRanks(): Unit = {

    val cards = Seq(
      Card(Diamond, A),
      Card(Heart, N(10)),
      Card(Club, K),
      Card(Heart, Q),
      Card(Club, J),
      Card(Club, N(10)),
      Card(Diamond, N(8))
    )

    new CombinationChecker(cards).check1RoyalFlush() shouldBe None
  }

  @Test
  def royalFlushNoByFlush(): Unit = {

    val cards = Seq(
      Card(Diamond, A),
      Card(Diamond, N(10)),
      Card(Diamond, K),
      Card(Spade, Q),
      Card(Diamond, J),
      Card(Diamond, N(10)),
      Card(Diamond, N(8))
    )

    new CombinationChecker(cards).check1RoyalFlush() shouldBe None
  }

  @Test
  def straightFlushFromAYes(): Unit = {

    val cards = Seq(
      Card(Diamond, N(5)),
      Card(Diamond, A),
      Card(Diamond, N(10)),
      Card(Club, K),
      Card(Diamond, N(2)),
      Card(Diamond, J),
      Card(Diamond, N(3)),
      Card(Diamond, N(4))
    )

    new CombinationChecker(cards).check2StraightFlush() shouldBe Some(Combination(
      Seq(
        Card(Diamond, N(5)),
        Card(Diamond, N(4)),
        Card(Diamond, N(3)),
        Card(Diamond, N(2)),
        Card(Diamond, A)),
      Seq(
        Card(Diamond, N(5)),
        Card(Diamond, N(4)),
        Card(Diamond, N(3)),
        Card(Diamond, N(2)),
        Card(Diamond, A)),
      90000000000L + 100000000 * 5))
  }

  @Test
  def straightFlushFromMiddleYes(): Unit = {

    val cards = Seq(
      Card(Diamond, Q),
      Card(Diamond, A),
      Card(Diamond, N(10)),
      Card(Club, K),
      Card(Club, N(8)),
      Card(Diamond, N(8)),
      Card(Diamond, J),
      Card(Diamond, N(9)),
      Card(Spade, J)
    )

    new CombinationChecker(cards).check2StraightFlush() shouldBe Some(Combination(
      Seq(
        Card(Diamond, Q),
        Card(Diamond, J),
        Card(Diamond, N(10)),
        Card(Diamond, N(9)),
        Card(Diamond, N(8))),
      Seq(
        Card(Diamond, Q),
        Card(Diamond, J),
        Card(Diamond, N(10)),
        Card(Diamond, N(9)),
        Card(Diamond, N(8))),
      90000000000L + 100000000 * 12))
  }

  @Test
  def straightFlushNoByRanks(): Unit = {

    val cards = Seq(
      Card(Diamond, N(2)),
      Card(Diamond, A),
      Card(Diamond, N(10)),
      Card(Club, K),
      Card(Club, N(8)),
      Card(Diamond, N(8)),
      Card(Diamond, J),
      Card(Diamond, N(9)),
      Card(Spade, J)
    )

    new CombinationChecker(cards).check2StraightFlush() shouldBe None
  }

  @Test
  def straightFlushNoBySuits(): Unit = {

    val cards = Seq(
      Card(Diamond, Q),
      Card(Diamond, A),
      Card(Club, N(10)),
      Card(Club, K),
      Card(Club, N(8)),
      Card(Diamond, N(8)),
      Card(Diamond, J),
      Card(Diamond, N(9)),
      Card(Spade, J)
    )

    new CombinationChecker(cards).check2StraightFlush() shouldBe None
  }

  @Test
  def kareYesKicker1(): Unit = {

    val cards = Seq(
      Card(Spade, Q),
      Card(Diamond, A),
      Card(Diamond, Q),
      Card(Club, K),
      Card(Club, N(8)),
      Card(Club, Q),
      Card(Spade, J),
      Card(Heart, Q)
    )

    new CombinationChecker(cards).check3Kare() shouldBe Some(Combination(
      Seq(
        Card(Spade, Q),
        Card(Diamond, Q),
        Card(Club, Q),
        Card(Heart, Q),
        Card(Diamond, A)),
      Seq(
        Card(Spade, Q),
        Card(Diamond, Q),
        Card(Club, Q),
        Card(Heart, Q)),
      80000000000L + 100000000 * 12 + 1000000 * 14))
  }

  @Test
  def kareYesKicker2(): Unit = {

    val cards = Seq(
      Card(Diamond, N(3)),
      Card(Diamond, J),
      Card(Club, Q),
      Card(Club, N(8)),
      Card(Diamond, K),
      Card(Club, K),
      Card(Spade, J),
      Card(Heart, K),
      Card(Spade, K)
    )

    new CombinationChecker(cards).check3Kare() shouldBe Some(Combination(
      Seq(
        Card(Diamond, K),
        Card(Club, K),
        Card(Heart, K),
        Card(Spade, K),
        Card(Club, Q)),
      Seq(
        Card(Diamond, K),
        Card(Club, K),
        Card(Heart, K),
        Card(Spade, K)),
      80000000000L + 100000000 * 13 + 1000000 * 12))
  }

  @Test
  def kareNo(): Unit = {

    val cards = Seq(
      Card(Diamond, N(3)),
      Card(Diamond, J),
      Card(Club, Q),
      Card(Club, N(8)),
      Card(Diamond, K),
      Card(Club, K),
      Card(Spade, J),
      Card(Heart, K),
      Card(Spade, A)
    )

    new CombinationChecker(cards).check3Kare() shouldBe None
  }

  @Test
  def fullHouseOne(): Unit = {

    val cards = Seq(
      Card(Diamond, N(3)),
      Card(Club, Q),
      Card(Club, N(8)),
      Card(Diamond, J),
      Card(Club, K),
      Card(Spade, J),
      Card(Heart, K),
      Card(Spade, K)
    )

    new CombinationChecker(cards).check4FullHouse() shouldBe Some(Combination(
      Seq(
        Card(Club, K),
        Card(Heart, K),
        Card(Spade, K),
        Card(Diamond, J),
        Card(Spade, J)),
      Seq(
        Card(Club, K),
        Card(Heart, K),
        Card(Spade, K),
        Card(Diamond, J),
        Card(Spade, J)), 70000000000L + 100000000 * 13 + 1000000 * 11))
  }

  @Test
  def fullHouseTwo(): Unit = {

    val cards = Seq(
      Card(Club, Q),
      Card(Diamond, J),
      Card(Club, K),
      Card(Spade, J),
      Card(Heart, K),
      Card(Club, N(8)),
      Card(Heart, J),
      Card(Diamond, N(3))
    )

    new CombinationChecker(cards).check4FullHouse() shouldBe Some(Combination(
      Seq(
        Card(Diamond, J),
        Card(Spade, J),
        Card(Heart, J),
        Card(Club, K),
        Card(Heart, K)),
      Seq(
        Card(Diamond, J),
        Card(Spade, J),
        Card(Heart, J),
        Card(Club, K),
        Card(Heart, K)),
      70000000000L + 100000000 * 11 + 1000000 * 13))
  }

  @Test
  def fullHouseNo(): Unit = {

    val cards = Seq(
      Card(Club, Q),
      Card(Diamond, J),
      Card(Club, K),
      Card(Spade, J),
      Card(Heart, K),
      Card(Club, N(8)),
      Card(Heart, Q),
      Card(Diamond, N(3))
    )

    new CombinationChecker(cards).check4FullHouse() shouldBe None
  }

  @Test
  def flash(): Unit = {

    val cards = Seq(
      Card(Club, Q),
      Card(Diamond, J),
      Card(Club, K),
      Card(Spade, J),
      Card(Club, N(8)),
      Card(Club, N(10)),
      Card(Heart, J),
      Card(Club, N(3))
    )

    new CombinationChecker(cards).check5Flush() shouldBe Some(Combination(
      Seq(
        Card(Club, K),
        Card(Club, Q),
        Card(Club, N(10)),
        Card(Club, N(8)),
        Card(Club, N(3))),
      Seq(
        Card(Club, K),
        Card(Club, Q),
        Card(Club, N(10)),
        Card(Club, N(8)),
        Card(Club, N(3))),
      60000000000L + 100000000 * 13 + 1000000 * 12 + 10000 * 10 + 100 * 8 + 3))

  }

  @Test
  def flashNo(): Unit = {

    val cards = Seq(
      Card(Club, Q),
      Card(Diamond, J),
      Card(Club, K),
      Card(Spade, J),
      Card(Heart, N(8)),
      Card(Club, N(10)),
      Card(Heart, J),
      Card(Club, N(3))
    )

    new CombinationChecker(cards).check5Flush() shouldBe None
  }

  @Test
  def staight(): Unit = {

    val cards = Seq(
      Card(Club, Q),
      Card(Diamond, J),
      Card(Club, K),
      Card(Spade, N(9)),
      Card(Club, N(8)),
      Card(Club, N(10)),
      Card(Heart, J),
      Card(Club, A)
    )

    new CombinationChecker(cards).check6Straight() shouldBe Some(Combination(
      Seq(
        Card(Club, A),
        Card(Club, K),
        Card(Club, Q),
        Card(Diamond, J),
        Card(Club, N(10))),
      Seq(
        Card(Club, A),
        Card(Club, K),
        Card(Club, Q),
        Card(Diamond, J),
        Card(Club, N(10))),
      50000000000L + 100000000 * 14))
  }

  @Test
  def staightNo(): Unit = {

    val cards = Seq(
      Card(Club, Q),
      Card(Diamond, J),
      Card(Club, K),
      Card(Spade, N(9)),
      Card(Club, N(8)),
      Card(Club, N(2)),
      Card(Heart, J),
      Card(Club, A)
    )

    new CombinationChecker(cards).check6Straight() shouldBe None
  }

  @Test
  def three(): Unit = {

    val cards = Seq(
      Card(Club, N(8)),
      Card(Spade, N(9)),
      Card(Diamond, N(8)),
      Card(Heart, N(8)),
      Card(Heart, J),
      Card(Club, A)
    )

    new CombinationChecker(cards).check7Three() shouldBe Some(Combination(
      Seq(
        Card(Club, N(8)),
        Card(Diamond, N(8)),
        Card(Heart, N(8)),
        Card(Club, A),
        Card(Heart, J)),
      Seq(
        Card(Club, N(8)),
        Card(Diamond, N(8)),
        Card(Heart, N(8))),
      40000000000L + 100000000 * 8 + 1000000 * 14 + 10000 * 11
    ))
  }

  @Test
  def threeNo(): Unit = {

    val cards = Seq(
      Card(Club, N(8)),
      Card(Diamond, J),
      Card(Club, K),
      Card(Spade, N(9)),
      Card(Diamond, N(8)),
      Card(Heart, N(9)),
      Card(Heart, J),
      Card(Club, A)
    )

    new CombinationChecker(cards).check7Three() shouldBe None
  }

  @Test
  def twoPair(): Unit = {

    val cards = Seq(
      Card(Heart, Q),
      Card(Club, N(8)),
      Card(Spade, N(9)),
      Card(Diamond, N(8)),
      Card(Heart, N(9)),
      Card(Club, K),
      Card(Heart, J)
    )

    new CombinationChecker(cards).check8TwoPair() shouldBe Some(Combination(
      Seq(
        Card(Spade, N(9)),
        Card(Heart, N(9)),
        Card(Club, N(8)),
        Card(Diamond, N(8)),
        Card(Club, K)),
      Seq(
        Card(Spade, N(9)),
        Card(Heart, N(9)),
        Card(Club, N(8)),
        Card(Diamond, N(8))),
      30000000000L + 100000000 * 9 + 1000000 * 8 + 10000 * 13))
  }


  @Test
  def twoPairNo(): Unit = {

    val cards = Seq(
      Card(Heart, Q),
      Card(Club, N(8)),
      Card(Spade, N(7)),
      Card(Diamond, N(8)),
      Card(Heart, N(9)),
      Card(Club, K),
      Card(Heart, J)
    )

    new CombinationChecker(cards).check8TwoPair() shouldBe None
  }

  @Test
  def pair(): Unit = {

    val cards = Seq(
      Card(Heart, Q),
      Card(Club, N(8)),
      Card(Spade, N(9)),
      Card(Diamond, N(3)),
      Card(Heart, N(3)),
      Card(Club, K),
      Card(Heart, J)
    )

    new CombinationChecker(cards).check9Pair() shouldBe Some(Combination(
      Seq(
        Card(Diamond, N(3)),
        Card(Heart, N(3)),
        Card(Club, K),
        Card(Heart, Q),
        Card(Heart, J)),
      Seq(
        Card(Diamond, N(3)),
        Card(Heart, N(3))),
      20000000000L + 100000000 * 3 + 1000000 * 13 + 10000 * 12 + 100 * 11))
  }

  @Test
  def pairNo(): Unit = {

    val cards = Seq(
      Card(Heart, Q),
      Card(Club, N(8)),
      Card(Spade, N(9)),
      Card(Diamond, N(3)),
      Card(Heart, N(2)),
      Card(Club, K),
      Card(Heart, J)
    )

    new CombinationChecker(cards).check8TwoPair() shouldBe None
  }

  @Test
  def highCard(): Unit = {

    println("Starting test high card...")

    val cards = Seq(
      Card(Heart, Q),
      Card(Club, N(8)),
      Card(Spade, N(9)),
      Card(Diamond, N(3)),
      Card(Heart, N(2)),
      Card(Club, K),
      Card(Heart, J)
    )

    new CombinationChecker(cards).check10HighCard() shouldBe Some(Combination(
      Seq(
        Card(Club, K),
        Card(Heart, Q),
        Card(Heart, J),
        Card(Spade, N(9)),
        Card(Club, N(8))),
      Seq(
        Card(Club, K)),
      10000000000L + 100000000 * 13 + 1000000 * 12 + 10000 * 11 + 100 * 9 + 8))

    println("Test high card ends")
  }

  @Deprecated
  def testPerformace(): Unit = {
    //1000000: 20 sec = 0.02ms per test
    for (i <- 1 to 100000)
      straightFlushFromAYes()
  }


}
