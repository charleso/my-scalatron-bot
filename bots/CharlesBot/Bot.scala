/**This material is intended as a community resource and is in the public domain.
 * Feel free to copy or modify it!
 */
package scalatron.botwar.botPlugin

import scala.util.Random

class ControlFunctionFactory {
  def create: (String => String) = new Bot().respond _
}

class Bot() {
  val rnd = new Random()
  var apocalypse: Int = 0
  var energy: Int = 0

  def respond(input: String): String = {
    val (opcode, params) = CommandParser.apply(input)

    if (opcode == "React") {
      val oldEnergy = energy
      energy = params("energy").toInt
      if (energy < oldEnergy) {
        Cmd("Say", Map("text" -> "Ouch!"))
      } else {
        val board = Board(params("view"))
        var moves = List(Avoid)
        val heuristics = moves.map(_.move(board))
        val options = heuristics.foldLeft(IndexedSeq(0, 0, 0, 0, 0, 0, 0, 0, 0)) {
          case (xs, x) => xs.zip(x).map {
            case (a, b) => a + b
          }
        }
        val newMove = options.zipWithIndex.sortBy(_._1).map(_._2).last
        val direction = newMove match {
          case 0 => XY(-1, -1)
          case 1 => XY(0, -1)
          case 2 => XY(1, -1)
          case 3 => XY(-1, 0)
          case 4 => XY(0, 0)
          case 5 => XY(1, 0)
          case 6 => XY(-1, 1)
          case 7 => XY(0, 1)
          case 8 => XY(1, 1)
        }
        //println(board.direct + " " + heuristics + " " + direction)

        Cmd("Move", Map("dx" -> direction.x, "dy" -> direction.y))
      }
    }

    else if (opcode == "Welcome") {
      apocalypse = params("apocalypse").toInt
      ""
    } else {
      ""
    }
  }

}

object CommandParser {
  /**"Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
  def apply(command: String): (String, Map[String, String]) = {
    /**"key=value" => ("key","value") */
    def splitParameterIntoKeyValue(param: String): (String, String) = {
      val segments = param.split('=')
      if (segments.length != 2)
        throw new IllegalStateException("parameter not a valid key/value pair: " + param)
      (segments(0), segments(1))
    }

    val segments = command.split('(')
    if (segments.length != 2)
      throw new IllegalStateException("invalid command: " + command)
    val opcode = segments(0)
    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
    (opcode, keyValuePairs)
  }
}


object Cmd {

  def main(args: Array[String]) {
    val me = Pos(31 * 31 / 2)
    for (y <- 0 until 31) {
      for (x <- 0 until 31) {
        val p = Pos((y * 31) + x)
        //print(p.toXY)
        print(me.distance(p).toString.substring(0, 1))
        print(" ")
      }
      println("")
    }
    println()
  }

  def apply(cmd: String, map: Map[String, Any]) = cmd + "(" + map.toList.map(a => a._1 + "=" + a._2).mkString(",") + ")"
}

object Board {
  val width = 31

  def apply(view: String) = {
    val b = view.map {
      case '_' => Empty
      case 'M' => Master
      case 'm' => Enemy
      case 'S' => Slave
      case 's' => Enemy
      case 'P' => Zugar
      case 'p' => Toxifera
      case 'B' => Fluppet
      case 'b' => Snorg
      case 'W' => Wall
      case '?' => Hidden
    }
    new Board(b)
  }
}


trait Move {

  def move(b: Board): IndexedSeq[Int] = {
    // TODO Use direct2 to look further ahead
    // TODO Only return 9 values instead of 18 etc
    flip(b.direct.map(weight))
  }

  /**
   * Adds positive weights to the opposite of negative squares - so we RUN AWAY!!!
   */
  def flip(i: IndexedSeq[Int]) = i.map(math.min(_, 0) * -1).reverse.zip(i).map {
    case (a, b) => a + b
  }

  def weight(s: S): Int = 0
}

object Avoid extends Move {
  override def weight(s: S) = s match {
    case Fluppet => 1
    case Zugar => 1
    case Wall => -1
    case Master => -1 // Don't stand still
    case Toxifera => -2
    case Snorg => -3
    case _ => 0
  }
}

case class Pos(i: Int) {

  import Board.width

  def toXY = XY(i % width, i / width)

  def distance(p: Pos) = {
    val us = toXY
    val them = p.toXY
    val x = them.x - us.x
    val y = them.y - us.y
    math.sqrt(x * x + y * y)
  }
}

class Board(s: IndexedSeq[S]) {

  import Board.width

  def get(i: Int) = s((width * width / 2) + i)

  def direct_(m: Int) = {
    val r = (i: Int) => (-m to m).map(_ + i)
    (-m to m).flatMap(x => r(x * width))
  }

  def directGet(m: Int) = direct_(m) map (get(_))

  def direct = directGet(1)

  def direct2 = directGet(2)

}

sealed trait S

case object Empty extends S

case object Master extends S

case object Slave extends S

case object Enemy extends S

case object Wall extends S

case object Fluppet extends S

case object Snorg extends S

case object Zugar extends S

case object Toxifera extends S

case object Hidden extends S

case class XY(x: Int, y: Int)

object XY {
  def randomUnit(rnd: Random) = XY(rnd.nextInt(3) - 1, rnd.nextInt(3) - 1)
}
