package de.htwg.se.mill.model.fieldComponent

import de.htwg.se.mill.model.fieldComponent._
import de.htwg.se.mill.model.Color
import org.junit.Assert._
import org.junit.Test


class StoneSpec {
  @Test
  def whiteStoneSetTest(): Unit =
    val whiteStoneSet = Stone("w+")
    assertEquals(Color.white, whiteStoneSet.whichColor)
    assertTrue(whiteStoneSet.isSet)

  @Test
  def whiteStoneNotSetTest(): Unit =
    val whiteStoneNotSet = Stone("w-")
    assertEquals(Color.white, whiteStoneNotSet.whichColor)
    assertFalse(whiteStoneNotSet.isSet)


  @Test
  def blackStoneSetTest(): Unit =
    val blackStoneSet = Stone("b+")
    assertEquals(Color.black, blackStoneSet.whichColor)
    assertTrue(blackStoneSet.isSet)

  @Test
  def blackStoneNotSetTest(): Unit =
    val blackStoneNotSet = Stone("b-")
    assertEquals(Color.black, blackStoneNotSet.whichColor)
    assertFalse(blackStoneNotSet.isSet)

  @Test
  def colorLessStoneNotSetTest(): Unit = {
    val colorLessStoneNotSetTest = Stone("")
    assertEquals(Color.noColor, colorLessStoneNotSetTest.whichColor)
    assertFalse(colorLessStoneNotSetTest.isSet)
  }
}
