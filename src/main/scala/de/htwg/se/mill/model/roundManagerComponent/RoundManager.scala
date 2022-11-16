package de.htwg.se.mill.model.roundManagerComponent

import de.htwg.se.mill.model._
import de.htwg.se.mill.model.fieldComponent.{Cell, Field}

import scala.math.abs

case class RoundManager(field: Field,
                        player1Mode: ModeState = SetModeState(),
                        player2Mode: ModeState = SetModeState(),
                        tmpCell: (Int, Int) = (-1, -1),
                        roundCounter: Int = 0,
                        borderToMoveMode: Int = 18,
                        update: (Int, Int) = (-1, -1),
                        winner: Int = 0):

  def this() = this(field = new Field(size = 7))

  def blackTurn(): Boolean = abs(roundCounter % 2) == 1

  def whiteTurn(): Boolean = abs(roundCounter % 2) == 0

  def handleClick(row: Int, col: Int): RoundManager =
    ((if (blackTurn()) player2Mode else player1Mode) match
      case RemoveModeState() => remove((row, col))
      case MoveModeState() => move((row, col))
      case FlyModeState() => fly((row, col))
      case SetModeState() | _ => set((row, col))
      ).checkWinner().modeChoice()

  def modeChoice(placedStones: (Int, Int) = (field.placedBlackStones(), field.placedWhiteStones())): RoundManager =
    val mgr: RoundManager = copy()
    var player1Mode = mgr.player1Mode
    var player2Mode = mgr.player2Mode
    val roundCounter = mgr.roundCounter
    val (placedBlackStones, placedWhiteStones) = placedStones

    if field.millState != MillState.handle(NoMillState()) then
      MillState.whichState(field.millState) match
        case WhiteMillState() => player1Mode = RemoveModeState()
        case BlackMillState() => player2Mode = RemoveModeState()
    else if roundCounter < borderToMoveMode then
      player1Mode = SetModeState()
      player2Mode = SetModeState()
      if roundCounter == borderToMoveMode - 1 then
        player1Mode = MoveModeState()
      end if
    else if placedBlackStones == 3 || placedWhiteStones == 3 then
      player1Mode = MoveModeState()
      player2Mode = MoveModeState()
      if placedWhiteStones == 3 then
        player1Mode = FlyModeState()
      end if
      if placedBlackStones == 3 then
        player2Mode = FlyModeState()
      end if
    else
      player1Mode = MoveModeState()
      player2Mode = MoveModeState()
    end if
    copy(player1Mode = player1Mode, player2Mode = player2Mode)
  end modeChoice

  private def set(cell: (Int, Int)): RoundManager =
    val mgr: RoundManager = copy()
    var field: Field = mgr.field
    var roundCounter: Int = mgr.roundCounter
    val (row, col): (Int, Int) = cell

    if field.available(row, col) && field.millState == "No Mill" then
      // set cell normally
      val (rField, rSuccessfullyRemoved) = field.set(row, col, if (mgr.blackTurn()) Cell("cb") else Cell("cw"))
      if rSuccessfullyRemoved then
        field = rField.checkMill(row, col)
        if field.millState == "No Mill" then
          roundCounter += 1
    copy(field = field, roundCounter = roundCounter)

  private def remove(cell: (Int, Int)): RoundManager =
    val mgr: RoundManager = copy()
    var field: Field = mgr.field
    var roundCounter: Int = mgr.roundCounter
    val (row, col): (Int, Int) = cell
    var update: Int = 0

    val cellColor: Color = field.cell(row, col).content.whichColor
    if (cellColor == Color.black && mgr.whiteTurn()) || (cellColor == Color.white && mgr.blackTurn()) then
      val ignore: Boolean = field.placedBlackStones() <= 3 && field.placedWhiteStones() <= 3
      val (rField, rSuccessfullyRemoved) = field.removeStone(row, col, ignore)
      if rSuccessfullyRemoved then
        field = rField.resetMill()
        roundCounter += 1
        update = 2
      end if
    copy(field = field, roundCounter = roundCounter)

  private def checkIfCanMove(): Boolean =
  // checks if the player has a cell where it can move to
  // if not it returns false
    if (whiteTurn() && player1Mode.handle == "MoveMode") || (blackTurn() && player2Mode.handle == "MoveMode") then
      val playerColor: Color = if (blackTurn()) Color.black else Color.white
      field.cellsWithIndex
        .filter(c => c._2.content.whichColor == playerColor)
        .exists(c => field.checkIfCanMove(c._1._1, c._1._2))
    else
      true

  private def checkIfHasOnly3Stones(): Boolean =
    if blackTurn() && player2Mode.handle != "SetMode" then
      field.placedBlackStones() < 3
    else if (whiteTurn() && player1Mode.handle != "SetMode") then
      field.placedWhiteStones() < 3
    else
      false

  private def checkWinner(): RoundManager =
    if (checkIfHasOnly3Stones() || !checkIfCanMove()) then
      val winner = if (blackTurn()) 2 else 1
      copy(winner = winner)
    else
      copy()

  private def move(cell: (Int, Int)): RoundManager = moveOrFly(cell, field.moveStone)

  private def fly(cell: (Int, Int)): RoundManager = moveOrFly(cell, field.fly)

  private def moveOrFly(cell: (Int, Int), changeStone: (Int, Int, Int, Int) => (Field, Boolean)): RoundManager =
    val mgr: RoundManager = copy()
    var field: Field = mgr.field
    var roundCounter: Int = mgr.roundCounter

    val (row, col): (Int, Int) = cell
    var tmpCell@(tmpRow, tmpCol): (Int, Int) = mgr.tmpCell

    val cellColor = field.cell(row, col).content.whichColor
    var update: Int = 0

    // do move only if it's own color and destination field has no color!!
    if (tmpCell != (-1, -1) && tmpCell != cell && cellColor == Color.noColor) then
      val (rField, rSuccessfullyMoved) = changeStone(tmpRow, tmpCol, row, col)
      if rSuccessfullyMoved then
        field = rField.checkMill(row, col)
        if field.millState == "No Mill" then
          roundCounter += 1
          tmpCell = (-1, -1) // reset temporary cell after move
          update = 2
        end if
      end if
    else
    // update temporary cell
      if (cellColor == Color.black && mgr.blackTurn()) ||
        (cellColor == Color.white && mgr.whiteTurn()) then
        tmpCell = cell
    copy(field = field, roundCounter = roundCounter, tmpCell = tmpCell, update = mgr.tmpCell)
  end moveOrFly