package de.htwg.se.mill.model.fieldComponent

import de.htwg.se.mill.model.fieldComponent._
import org.junit.Assert._
import org.junit.Test

class CellSpec:
  @Test
  def whiteCellTest(): Unit =
    val whiteCell = Cell("cw")
    assertEquals("White Stone", whiteCell.toString)
    assertEquals(Stone("w+").color, whiteCell.content.color)
    assertTrue(whiteCell.filled)

  @Test
  def blackCellTest(): Unit =
    val blackCell = Cell("cb")
    assertEquals("Black Stone", blackCell.toString)
    assertEquals(Stone("b+").color, blackCell.content.color)
    assertTrue(blackCell.filled)

  @Test
  def emptyCellTest(): Unit =
    val emptyCell = Cell("ce")
    assertEquals("No Stone", emptyCell.toString)
    assertEquals(Stone("").color, emptyCell.content.color)
    assertFalse(emptyCell.filled)

