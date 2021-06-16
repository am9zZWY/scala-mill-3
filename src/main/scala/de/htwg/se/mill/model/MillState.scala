package de.htwg.se.mill.model

trait MillState:
  def handle: String

case class WhiteMillState() extends MillState:
  override def handle: String = "White Mill"

case class BlackMillState() extends MillState:
  override def handle: String = "Black Mill"

case class NoMillState() extends MillState:
  override def handle: String = "No Mill"

object MillState:
  def handle(e: MillState): String =
    (e match
      case WhiteMillState() => WhiteMillState()
      case BlackMillState() => BlackMillState()
      case _ => NoMillState()
    ).handle

  def whichState(s: String): MillState =
    s match
      case "White Mill" => WhiteMillState()
      case "Black Mill" => BlackMillState()
      case _ => NoMillState()
  