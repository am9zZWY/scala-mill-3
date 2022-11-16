package de.htwg.se.mill.controller

import de.htwg.se.mill.util.{Observable, UndoManager}
import de.htwg.se.mill.controller.GameState._
import de.htwg.se.mill.model._
import de.htwg.se.mill.model.fieldComponent.{FieldCreateRandomStrategy, FieldCreator}
import de.htwg.se.mill.model.roundManagerComponent.RoundManager

class Controller extends Observable:
  var gameState: GameState = IDLE
  var roundManager = RoundManager(new FieldCreator().createField(7))

  def handleClick(row: Int, col: Int) =
    roundManager = roundManager.handleClick(row, col)
    notifyObservers
  
  def createEmptyField(size: Int): Unit =
    roundManager = roundManager.copy(field = new FieldCreator().createField(7)) 
    notifyObservers

  def createRandomField(size: Int, amoutStones:Int): Unit =
    roundManager = roundManager.copy((new FieldCreateRandomStrategy).createNewField(size))
    notifyObservers

  def fieldToString: String = roundManager.field.toString
