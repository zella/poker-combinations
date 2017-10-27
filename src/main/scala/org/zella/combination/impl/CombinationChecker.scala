package org.zella.combination.impl

import org.zella.cards.Ranks._
import org.zella.cards.{Card, ICard, Weight}
import org.zella.combination.{Combination, ICombinationChecker}

import scala.collection.mutable

/**
  *
  * Check cards for combination
  *
  * @author zella.
  */
class CombinationChecker(cards: Seq[ICard]) extends ICombinationChecker {

  import CombinationChecker._

  //умножатели весов карт
  private val MULT_1 = 100000000
  private val MULT_2 = 1000000
  private val MULT_3 = 10000
  private val MULT_4 = 100
  private val MULT_5 = 1

  lazy val sortedByRank: mutable.LinkedHashSet[ICard] = {
    mutable.LinkedHashSet(cards.sorted(Ordering[Weight].reverse): _*)
  }
  lazy val sortedByRankIndexed: IndexedSeq[ICard] = sortedByRank.toIndexedSeq

  def sortByRankTyzLower(cards: Seq[ICard]): Seq[ICard] = cards.sortWith((c1, c2) => {
    val r1 = if (c1.rank.equals(A)) N(1) else c1.rank
    val r2 = if (c2.rank.equals(A)) N(1) else c2.rank
    r1 > r2
  })

  def subSeqBasedOnRank(seq: Seq[ICard], ranks: Seq[Rank]): Option[Seq[ICard]] = {
    //реверс, для того чтобы с большей карты слева начинали
    seq.map(_.rank).indexOfSlice(ranks) match {
      case -1 => None
      case idx => Some(seq.slice(idx, idx + 5))
    }
  }

  def groupByRankOrdered(source: Seq[ICard]): Seq[(Rank, Seq[ICard])] = {
    source.groupBy(_.rank).toSeq.sortBy(_._1).reverse
  }

  private def isStraightInternal(sorted: Seq[ICard], baseWeight: Long): Option[Combination] = this.synchronized {

    val withoutDuplicates = groupByRankOrdered(sorted).map(_._2.head)

    var test = subSeqBasedOnRank(withoutDuplicates, Seq(N(10), J, Q, K, A).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + A.weight * MULT_1))
    test = subSeqBasedOnRank(withoutDuplicates, Seq(N(9), N(10), J, Q, K).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + K.weight * MULT_1))
    test = subSeqBasedOnRank(withoutDuplicates, Seq(N(8), N(9), N(10), J, Q).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + Q.weight * MULT_1))
    test = subSeqBasedOnRank(withoutDuplicates, Seq(N(7), N(8), N(9), N(10), J).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + J.weight * MULT_1))
    test = subSeqBasedOnRank(withoutDuplicates, Seq(N(6), N(7), N(8), N(9), N(10)).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + N(10).weight * MULT_1))
    test = subSeqBasedOnRank(withoutDuplicates, Seq(N(5), N(6), N(7), N(8), N(9)).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + N(9).weight * MULT_1))
    test = subSeqBasedOnRank(withoutDuplicates, Seq(N(4), N(5), N(6), N(7), N(8)).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + N(8).weight * MULT_1))
    test = subSeqBasedOnRank(withoutDuplicates, Seq(N(3), N(4), N(5), N(6), N(7)).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + N(7).weight * MULT_1))
    test = subSeqBasedOnRank(withoutDuplicates, Seq(N(2), N(3), N(4), N(5), N(6)).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + N(6).weight * MULT_1))
    test = subSeqBasedOnRank(sortByRankTyzLower(withoutDuplicates), Seq(A, N(2), N(3), N(4), N(5)).reverse)
    if (test.isDefined) return test.map(seq => Combination(seq, seq, baseWeight + N(5).weight * MULT_1))
    None
  }

  /**
    * Check cards for royal flush
    *
    * @return (5 cards combination, weight)
    */
  override def check1RoyalFlush(): Option[Combination] = {

    val sameSuits5 = sortedByRank.groupBy(_.suit).find(_._2.size >= 5).map(_._2.take(5).toSeq)

    sameSuits5 match {
      case Some(Seq(Card(_, A), Card(_, K), Card(_, Q), Card(_, J), Card(_, N(10)))) =>
        sameSuits5.map(seq => Combination(seq, seq, WEIGHT_ROYALFLUSH))
      case _ => None
    }
  }


  override def check2StraightFlush(): Option[Combination] = {
    val sameSuits = sortedByRank.groupBy(_.suit).find(_._2.size >= 5).map(_._2.toSeq)
    sameSuits match {
      case Some(_cards) => isStraightInternal(_cards, WEIGHT_STRAIGHTFLUSH)
      case _ => None
    }
  }


  override def check3Kare(): Option[Combination] = {

    val fourSeq = groupByRankOrdered(sortedByRankIndexed).find(_._2.size == 4).map(_._2)

    fourSeq match {
      case None => None
      case Some(_four) =>

        //sortedByRanks : A Q Q Q Q 10 9
        //four: Q Q Q Q
        //sortedByRanks diff pair: A 10 9
        //four + diff: Q Q Q Q A 10 9
        //take 5: Q Q Q Q A
        val comb = (_four ++ sortedByRank.diff(_four.toSet)).take(5).toIndexedSeq
        Some(Combination(comb, comb.take(4), WEIGHT_KARE +
          comb(1).rank.weight * MULT_1 +
          comb(4).rank.weight * MULT_2
        ))
    }
  }


  override def check4FullHouse(): Option[Combination] = {

    val threeSeq = groupByRankOrdered(sortedByRankIndexed).find(_._2.size == 3).map(_._2)

    threeSeq match {
      case None => None
      case Some(present) =>
        val other = sortedByRank.diff(present.toSet)
        val twoSeq = groupByRankOrdered(other.toSeq).find(_._2.size == 2).map(_._2)
        twoSeq match {
          case None => None
          case Some(presentAlso) =>

            //sortedByRanks : A Q Q J J J 9
            //threeSeq J J J
            //twoSeq Q Q
            //threeSeq + twoSeq + last: J J J Q Q A 10 9
            //take 5: J J J Q Q

            val comb = (present ++ presentAlso).toIndexedSeq

            Some(Combination(comb, comb, WEIGHT_FULL_HOUSE +
              comb(0).rank.weight * MULT_1 +
              comb(3).rank.weight * MULT_2
            ))
        }
    }
  }


  override def check5Flush(): Option[Combination] = {

    sortedByRank
      .groupBy(_.suit)
      .find(_._2.size >= 5)
      .map(_._2.take(5).toIndexedSeq)
      .map(_cards => Combination(_cards, _cards,
        WEIGHT_FLUSH +
          _cards(0).rank.weight * MULT_1 +
          _cards(1).rank.weight * MULT_2 +
          _cards(2).rank.weight * MULT_3 +
          _cards(3).rank.weight * MULT_4 +
          _cards(4).rank.weight * MULT_5)
      )
  }


  override def check6Straight(): Option[Combination] = {
    isStraightInternal(sortedByRankIndexed, WEIGHT_STRAIGHT)
  }


  override def check7Three(): Option[Combination] = {

    val threeSeq = groupByRankOrdered(sortedByRankIndexed).find(_._2.size == 3).map(_._2)

    threeSeq match {
      case None => None
      case Some(_three) =>

        //sortedByRanks : A Q Q Q J 10 9
        //three: Q Q Q
        //sortedByRanks diff pair: A J 10 9
        //three + diff: Q Q Q A J 10 9
        //take 5: Q Q Q A J
        val comb = (_three ++ sortedByRank.diff(_three.toSet)).take(5).toIndexedSeq
        Some(Combination(comb, comb.take(3), WEIGHT_THREE +
          comb(0).rank.weight * MULT_1 +
          comb(3).rank.weight * MULT_2 +
          comb(4).rank.weight * MULT_3
        ))
    }
  }


  override def check8TwoPair(): Option[Combination] = {

    val twoSeqOne = groupByRankOrdered(sortedByRankIndexed).find(_._2.size == 2).map(_._2)

    twoSeqOne match {
      case None => None
      case Some(present) =>
        val other = sortedByRank.diff(present.toSet)
        val twoSeq = groupByRankOrdered(other.toSeq).find(_._2.size == 2).map(_._2)
        twoSeq match {
          case None => None
          case Some(presentAlso) =>

            //sortedByRanks : A Q Q J J 10 9
            //twoSeqOne Q Q
            //twoSeq J J
            //sortedByRanks diff twoSeqOne: A J J 10 9
            //last diff twoSeq = A 10 9
            //twoSeqOne + twoSeq + last: Q Q J J A 10 9
            //take 5: Q Q J J A

            val res1 = sortedByRank.diff(present.toSet)

            val res2 = res1.diff(presentAlso.toSet)

            val comb = (present ++ presentAlso ++ res2).take(5).toIndexedSeq

            Some(Combination(comb, comb.take(4), WEIGHT_TWO_PAIR +
              comb(0).rank.weight * MULT_1 +
              comb(2).rank.weight * MULT_2 +
              //kicker
              comb(4).rank.weight * MULT_3
            ))
        }
    }
  }


  override def check9Pair(): Option[Combination] = {
    val pair = groupByRankOrdered(sortedByRankIndexed).find(_._2.size == 2).map(_._2)

    pair match {
      case None => None
      case Some(_pair) =>
        //sortedByRanks : A K Q Q J 10 9
        //pair: Q Q
        //sortedByRanks diff pair: A K J 10 9
        //pair + diff: Q Q A K J 10 9
        //take 5: Q Q A K J
        val comb = (_pair ++ sortedByRank.diff(_pair.toSet)).take(5).toIndexedSeq
        Some(Combination(comb, comb.take(2), WEIGHT_ONE_PAIR +
          comb(0).rank.weight * MULT_1 +
          comb(2).rank.weight * MULT_2 +
          comb(3).rank.weight * MULT_3 +
          comb(4).rank.weight * MULT_4
        ))
    }
  }


  override def check10HighCard(): Option[Combination] = {

    Some(Combination(sortedByRankIndexed.take(5), sortedByRankIndexed.take(1), WEIGHT_HIGHCARD +
      sortedByRankIndexed(0).rank.weight * MULT_1 +
      sortedByRankIndexed(1).rank.weight * MULT_2 +
      sortedByRankIndexed(2).rank.weight * MULT_3 +
      sortedByRankIndexed(3).rank.weight * MULT_4 +
      sortedByRankIndexed(4).rank.weight * MULT_5
    ))
  }
}

object CombinationChecker {

  //веса комбинации
  val WEIGHT_ROYALFLUSH = 100000000000L
  val WEIGHT_STRAIGHTFLUSH = 90000000000L
  val WEIGHT_KARE = 80000000000L
  val WEIGHT_FULL_HOUSE = 70000000000L
  val WEIGHT_FLUSH = 60000000000L
  val WEIGHT_STRAIGHT = 50000000000L
  val WEIGHT_THREE = 40000000000L
  val WEIGHT_TWO_PAIR = 30000000000L
  val WEIGHT_ONE_PAIR = 20000000000L
  val WEIGHT_HIGHCARD = 10000000000L

}
