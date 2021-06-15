package de.htwg.se.mill.controller.controllerComponent

import scala.concurrent.Future
import scala.swing.Publisher
import scala.swing.event.Event

trait ControllerInterface extends Publisher {
  var gameState:String
  // var cachedField: Option[JsValue]
  def createPlayer(name: String, number: Int): Future[String]
  def createEmptyField(size: Int): Unit
  def createRandomField(size: Int): Unit
  def fieldToString(oncomplete: Option[String] => Unit): Unit
  def fieldToHtml(oncomplete: Option[String] => Unit): Unit
  def fieldToHtmlSync: String
  def fieldToJson(oncomplete: Option[String] => Unit): Unit

  def getRoundCounter(oncomplete: Option[String] => Unit): Unit
  def handleClick(row: Int, column: Int)(oncomplete: Option[String] => Unit): Unit
  def undo(): Unit
  def redo(): Unit
  def changeSaveMethod(method: String): Unit
  def save(): Unit
  def load(): Unit
  def saveDB(): Unit
  def loadDB(id: Int): Unit
  def color(row: Int, col: Int)(oncomplete: Option[String] => Unit): Unit
  def isSet(row:Int, col:Int)(oncomplete: Option[String] => Unit): Unit
  def possiblePosition(row:Int, col:Int): Boolean
  def fieldsize:Int
  def getWinner(oncomplete: Option[String] => Unit): Unit
  def getWinnerText(oncomplete: Option[String] => Unit): Unit
  def getMillState(oncomplete: Option[String] => Unit): Unit
}

case class TwoCellsChanged(cell: (Int, Int)) extends Event
class CellChanged extends Event // could be case class
class FieldChanged extends Event
