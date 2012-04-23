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
  var lastMove = 0;

  def respond(input: String): String = {
    val (opcode, params) = CommandParser.apply(input)

    if (opcode == "React") {
      var cmds = List[Cmd.Cmd]()
      val oldEnergy = energy
      energy = params("energy").toInt
      if (energy < oldEnergy) {
        cmds ::= ("Say", Map("text" -> "Ouch!"))
      }
      val board = Board(params("view"))
      var moves = List(Avoid)
      val horizon = 15
      val heuristics = moves.map(m => board.map(horizon, m.weight))
      val options = heuristics.foldLeft(IndexedSeq(0, 0, 0, 0, 0, 0, 0, 0, 0)) {
        case (xs, x) => xs.zip(x).map {
          case (a, b) => a + b
        }
      }
      val o2 = options.updated(lastMove, options(lastMove) * 5)
      val newMove = board.flip(o2, 0.3f).zipWithIndex.sortBy(_._1).map(_._2).last
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
      //board.printBoard(horizon)
      //println(heuristics + " " + direction + " " + lastMove)
      lastMove = newMove

      val move = ("Move", Map("direction" -> (direction.x + ":" + direction.y)))
      Cmd(move :: cmds)
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
  type Cmd = (String, Map[String, Any])

  def apply(cmds: List[Cmd]) = cmds.map {
    case (cmd, map) => cmd + "(" + map.toList.map(a => a._1 + "=" + a._2).mkString(",") + ")"
  }.mkString("|")
}

object Board {
  val width = 31
  val types = List(Empty, Master, MiniMe, Slave, Enemy, Wall, Fluppet, Snorg, Zugar, Toxifera, Hidden).map(s => (s.c, s)).toMap

  def apply(view: String) = new Board(view.map(types(_)))
}


trait Move {

  def weight(s: S): Int = 0
}

object Avoid extends Move {
  override def weight(s: S) = s match {
    case Fluppet => 13
    case Zugar => 12
    case Wall => -5
    case Slave => -11
    case Enemy => -10
    case Toxifera => -2
    case Snorg => -10
    case _ => 0
  }
}

abstract class S(val c: Char)

case object Empty extends S('_')

case object Master extends S('M')

case object Slave extends S('m')

case object MiniMe extends S('S')

case object Enemy extends S('s')

case object Wall extends S('W')

case object Fluppet extends S('B')

case object Snorg extends S('b')

case object Zugar extends S('P')

case object Toxifera extends S('p')

case object Hidden extends S('?')

case class Pos(i: Int) {

  import Board.width

  val toXY = XY(i % width, i / width)

  def vector(p: Pos) = {
    val us = toXY
    val them = p.toXY
    val x = them.x - us.x
    val y = them.y - us.y
    def round(i: Int): Int = if (i == 0) 0 else math.round(i / math.abs(i))
    val dir = toDirection(round(x), round(y))
    val dist = math.round(math.sqrt(x * x + y * y))
    (dir, dist)
  }

  def toDirection(x: Int, y: Int) = (x, y) match {
    case (-1, -1) => 0
    case (0, -1) => 1
    case (1, -1) => 2
    case (-1, 0) => 3
    case (0, 0) => 4
    case (1, 0) => 5
    case (-1, 1) => 6
    case (0, 1) => 7
    case (1, 1) => 8
  }

}

class Board(s: IndexedSeq[S]) {

  import Board.width

  def direct_(m: Int) = {
    val r = (i: Int) => (-m to m).map(_ + i)
    (-m to m).flatMap(x => r(x * width)) map (_ + (width * width / 2))
  }

  def directGet(m: Int) = direct_(m) map (s(_))


  def printBoard(m: Int) {
    val b = directGet(m)
    val n = (m * 2) + 1

    for (y <- 0 until n) {
      for (x <- 0 until n) {
        print(b(x + (n * y)).c)
      }
      println("")
    }
  }


  def direct = directGet(1)

  def map(i: Int, f: S => Int) = {
    val me = Pos(width * width / 2)
    val a = direct_(i) map {
      p => {
        val v = me.vector(Pos(p))
        (v._1, f(s(p)) * math.pow(i - v._2 + 1, 5))
      }
    } groupBy (_._1) mapValues (_.map(_._2).sum.toInt)
    (0 to 8).map(i => a.getOrElse(i, 0))
  }

  /**
   * Adds positive weights to the opposite of negative squares - so we RUN AWAY!!!
   */
  def flip(i: IndexedSeq[Int], f: Float) = i.map(m => math.round(m * -f)).reverse.zip(i).map {
    case (a, b) => a + b
  }

}

case class XY(x: Int, y: Int)

object XY {
  def randomUnit(rnd: Random) = XY(rnd.nextInt(3) - 1, rnd.nextInt(3) - 1)
}
