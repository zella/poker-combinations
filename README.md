# poker-combinations
Scala library for resolving texas holdem combinations

[![](https://jitpack.io/v/zella/poker-combinations.svg)](https://jitpack.io/#zella/poker-combinations)

 ## Usage
    
**Argument 1:**
 Map of players identifiers and their cards
    
    val playersAndCards = Map[String, Seq[ICard]](
      "1" -> Seq(Card(Heart, A), Card(Diamond, K)),
      "2" -> Seq(Card(Heart, N(10)), Card(Diamond, N(10))),
      "3" -> Seq(Card(Club, Q), Card(Diamond, J)),
      "4" -> Seq(Card(Diamond, A), Card(Heart, K)),
      "5" -> Seq(Card(Diamond, N(2)), Card(Spade, N(2)))
    )
 **Argument 2:**
 Table cards  
   
    val tableCards = Seq(
      Card(Heart, N(2)),
      Card(Heart, N(3)),
      Card(Spade, N(10)),
      Card(Heart, J),
      Card(Club, N(9))
    )
    
 
    val resolver = CombinationResolver[String]() //thread safe instance

    val computed: Seq[(String,Combination)] = resolver.resolve(playersAndCards, tableCards)`
    
  returns sequens of all players combinations.
    
   Where `String` - YourPlayerIdType
  
  `Combination` -
   
    cards - 5 cards  
    combination - cards that reflects combination type. Ex: if [KKQQ7] then combiantion = [KKQQ]  
    weight - weight in `Long`
    
 


