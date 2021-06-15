package de.htwg.se.mill.controller

import de.htwg.se.mill.model.{Cell, Field, FieldCreateRandomStrategy, FieldCreator}
import de.htwg.se.mill.util.{Observable, UndoManager}
import de.htwg.se.mill.controller.GameState._

class Controller(var field:Field) extends Observable:

  private val undoManager = new UndoManager
  var gameState: GameState = IDLE
  var roundManager = null

  def handleClick(row: Int, col: Int) =
    roundManager = roundManager.handleClick(row, col)
    notifyObservers
  
  def createEmptyField(size: Int): Unit =
    roundManager = roundManager.copy(field = new FieldCreator().createField(7)) 
    notifyObservers

  def createRandomField(size: Int, amoutStones:Int): Unit =
    field = (new FieldCreateRandomStrategy).createNewField(size)
    notifyObservers

  def fieldToString: String = field.toString

  def set(row: Int, col: Int, c: Cell): Unit =
    undoManager.doStep(new SetCommand(row, col, c, this))
    notifyObservers

  def undo: Unit =
    undoManager.undoStep()
    notifyObservers

  def redo: Unit =
    undoManager.redoStep()
    notifyObservers
