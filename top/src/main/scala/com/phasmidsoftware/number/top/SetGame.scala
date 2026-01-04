/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.top

import scala.util.Random

/**
  * Represents the game logic for the card game "Set".
  * See [[https://en.wikipedia.org/wiki/Set_(card_game)]].
  *
  * The SetGame class encapsulates functionality for managing and executing the rules
  * and behavior of the game. The game involves identifying sets of three cards
  * from a collection that satisfy specific matching conditions.
  *
  * This class may include operations such as dealing cards, validating sets,
  * and tracking the state of the game. It is designed to support
  * the core mechanics of the "Set" game.
  */
class SetGame(random: Random = new Random) {

  /**
    * Deals a random selection of 12 cards from the full deck.
    * The method shuffles the complete sequence of cards and selects
    * the first 12 cards in the newly randomized order.
    * HINT: use `random` to do the shuffling.
    *
    * @return A sequence of 12 randomly selected `Card` objects.
    */
  def dozen: Seq[Card] = random.shuffle(fullDeck).take(12)

  /**
    * Identifies all possible valid sets from a given sequence of cards.
    * A valid set consists of exactly three cards that satisfy the rules of the "Set" game:
    * for each attribute (shape, color, number, and shading), the values must either all be the same
    * or all be different across the three cards.
    *
    * The method generates all possible combinations of three cards from the input sequence,
    * evaluates each combination to check if it forms a valid set, and returns the collection of valid sets.
    *
    * @param cards A sequence of `Card` objects to search for valid sets.
    * @return A sequence of valid `Set` objects found within the given cards.
    */
  def sets(cards: Seq[Card]): Seq[Set] =
    (for {
      i <- cards.indices
      j <- Range(i + 1, cards.size)
      k <- Range(j + 1, cards.size)
    } yield Set(Seq(cards(i), cards(j), cards(k)))).filter(_.isSet)

  /**
    * Generates a full deck of 81 unique cards for the game of Set.
    */
  val fullDeck: Seq[Card] = for {
    number <- Number.values.toSeq
    shape <- Shape.values.toSeq
    color <- Color.values.toSeq
    shading <- Shading.values.toSeq
  } yield Card(number, shape, color, shading)
}

@main
def main(args: String *): Unit = {
  val game = new SetGame
  val allCards = game.fullDeck
  println(s"There are ${allCards.size} card in all: ${allCards.mkString(", ")}")
  val sets = game.sets(game.dozen)
  println(s"There are ${sets.size} possible sets: ${sets.mkString(", ")}")
}

/**
  * Represents a set of three cards in the game of Set.
  * A valid `Set` consists of exactly three cards, where for each attribute
  * (shape, color, number, and shading), the values must either all be the same
  * or all be different across the three cards.
  *
  * @constructor Creates a `Set` with a specified sequence of three cards.
  *              Ensures the sequence contains exactly three cards.
  * @param cards The sequence of three `Card` objects that make up the set.
  */
case class Set(cards: Seq[Card]) {
  require(cards.size == 3)

  // Lens functions:
  val number: Card => Int = _.number.toInt
  val shape: Card => Int = _.shape.toInt
  val color: Card => Int = _.color.toInt
  val shading: Card => Int = _.shading.toInt

  /**
    * Checks if the three cards in the set form a valid `Set`.
    * A valid `Set` is formed when, for each attribute (shape, color, number, shading),
    * the values are either all the same or all different across the three cards.
    *
    * @return `true` if the cards form a valid `Set`, `false` otherwise.
    */
  def isSet: Boolean =
    attributesMatch(shape) && attributesMatch(color) && attributesMatch(number) && attributesMatch(shading)

  /**
    * Checks whether the values of a particular attribute across the three cards
    * are either all the same or all different.
    *
    * @param f A lens function that extracts the specific attribute from a card
    *          (e.g., shape, color, number, or shading).
    * @return `true` if the values of the specified attribute on the three cards
    *         are either all identical or all unique, otherwise `false`.
    */
  def attributesMatch(f: Card => Int): Boolean = {
    val styles = cards.map(f).distinct.size
    styles == 1 || styles == 3
  }

  override def toString: String = s"{${cards.mkString(", ")}}"
}

/**
  * A trait representing a generic attribute with an integer mapping.
  *
  * Implementations of this trait are expected to provide a concrete 
  * definition of the `toInt` method, which maps the attribute to a
  * corresponding integer value. This may be used for comparison, 
  * serialization, or other domain-specific purposes.
  */
trait Attribute:
  def toInt: Int

enum Color(val toInt: Int) extends Attribute:
  case Red extends Color(1)
  case Green extends Color(2)
  case Purple extends Color(3)

enum Shape(val toInt: Int) extends Attribute:
  case Oval extends Shape(1)
  case Diamond extends Shape(2)
  case Squiggle extends Shape(3)

enum Shading(val toInt: Int) extends Attribute:
  case Solid extends Shading(1)
  case Striped extends Shading(2)
  case Open extends Shading(3)

enum Number(val toInt: Int) extends Attribute:
  case One extends Number(1)
  case Two extends Number(2)
  case Three extends Number(3)

/**
  * Represents a card with specific attributes.
  *
  * @param shape   The shape attribute of the card, represented as an integer.
  *                There are three shapes: diamond (1), oval (2), and squiggle (3).
  * @param color   The color attribute of the card, represented as an integer.
  *                There are three colors: red (1), green (2), and purple (3).
  * @param number  The number attribute of the card, represented as an integer from 1 to 3.
  * @param shading The shading attribute of the card, represented as an integer.
  *                There are three shadings: solid (1), striped (2), and open (3).
  */
case class Card(number: Number, shape: Shape, color: Color, shading: Shading):
  override def toString: String = s"$number:$shape:$color:$shading"
