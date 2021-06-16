package de.htwg.se.mill.model.fieldComponent

trait CellTrait(val filled: Boolean, val content: Stone, val colorAsChar: Char)

private class WhiteStoneCell extends CellTrait(true, Stone("w+"), 'w') :
  override def toString: String = "White Stone"

private class BlackStoneCell extends CellTrait(true, Stone("b+"), 'b') :
  override def toString: String = "Black Stone"

private class EmptyCell extends CellTrait(false, Stone("n"), 'o') :
  override def toString: String = "No Stone"

object Cell:
  def apply(kind: String): CellTrait = kind match {
    case "cw" => new WhiteStoneCell()
    case "cb" => new BlackStoneCell()
    case _ => new EmptyCell()
  }