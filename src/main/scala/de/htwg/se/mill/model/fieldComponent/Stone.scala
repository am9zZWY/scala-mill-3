package de.htwg.se.mill.model.fieldComponent

import de.htwg.se.mill.model.Color

trait Stone(val color: Color):
  def whichColor: Color = color
  def isSet: Boolean

private class WhiteStone(value: Int) extends Stone(Color.white):
  override def isSet: Boolean = value != 0

private class BlackStone(value: Int) extends Stone(Color.black):
  override def isSet: Boolean = value != 0

private class ColorLessStone(value: Int) extends Stone(Color.noColor):
  override def isSet: Boolean = value != 0

object Stone:
  def apply(kind: String): Stone = kind match
    case "w+" => new WhiteStone(1)
    case "w-" => new WhiteStone(0)
    case "b+" => new BlackStone(1)
    case "b-" => new BlackStone(0)
    case _ => new ColorLessStone(0)