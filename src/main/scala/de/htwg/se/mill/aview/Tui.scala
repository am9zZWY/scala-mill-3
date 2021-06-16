package de.htwg.se.mill.aview

import de.htwg.se.mill.controller.{Controller, GameState}
import de.htwg.se.mill.model.Color
import de.htwg.se.mill.util.Observer


class Tui(controller: Controller) extends Observer:

  controller.add(this)
  val size = 7
  val amountStones = 6

  def execInput(input: String): Unit =
    input match {
      case "new" => controller.createEmptyField(size)
      case "random" => controller.createRandomField(size, amountStones)
      case _ =>
        input.toList.filter(p => p != ' ').filter(_.isDigit).map(p => p.toString.toInt) match {
          case row :: column :: Nil =>
            controller.handleClick(row, column)
            "valid command: " + input
          case _ =>
            "Wrong input: " + input
        }
    }

  override def update: Boolean =
    println(controller.fieldToString)
    println(GameState.message(controller.gameState))
    controller.gameState = GameState.IDLE
    true
