package de.htwg.se.mill

import de.htwg.se.mill.aview.Tui
import de.htwg.se.mill.controller.Controller

import scala.io.StdIn.readLine

object Mill {
  val controller = new Controller()
  val tui = new Tui(controller)
  controller.notifyObservers
  
  def main(args: Array[String]): Unit = {
    var input:String = ""

    while (input != "exit")
    do
      printf("Possible commands: new, random, white, black, exit  -->")
      input = readLine()
      tui.execInput(input)
  }

}
