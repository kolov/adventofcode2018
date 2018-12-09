package example

// See https://adventofcode.com/2018/day/9

object Day9Part1 extends App {

  case class Game(highestMarble: Int, highScore: Int, marbles: Array[Int], current: Int, scores: Array[Int]) {
    def player = ( ( highestMarble - 1 ) % scores.size ) + 1

    def show =
      println(s"[${player + 1}] " +
        marbles.zipWithIndex.map { case (v: Int, i: Int) => if (i == current) s"($v)" else s"$v" }.mkString(" ") +
        s" [$highScore]")

    def play: Game = {
      val marbleNummber = highestMarble + 1
      val size = marbles.size
      if (marbleNummber % 23 != 0) {
        val pos = ( current + 1 ) % size
        val newMarbles = ( marbles.slice(0, pos + 1) :+ marbleNummber ) ++ marbles.slice(pos + 1, size)
        Game(marbleNummber, highScore, newMarbles, pos + 1, scores)
      } else {
        val pos = ( size + current - 7 ) % size
        val player = ( marbleNummber - 1 ) % scores.size

        val points = marbles(pos)
        val newMarbles = marbles.slice(0, pos) ++ marbles.slice(pos + 1, size)
        val newScore = scores(player) + points + marbleNummber
        val newScores = scores.updated(player, newScore)
        Game(marbleNummber, Math.max(newScore, highScore), newMarbles, pos, newScores)
      }
    }

  }

  def game0(players: Int) = Game(0, 0, Array(0), 0, ( 1 to players ).foldLeft(Array[Int]())((v, _) => v :+ 0))


  var start = System.currentTimeMillis

  def points(g: Game): Stream[Game] = g #:: points(g.play)

  def solve(players: Int, lastMarble: Int) =
    println(s"The high score after marble $lastMarble was ${points(game0(players)).dropWhile(_.highestMarble < lastMarble).head.highScore}")

  solve(10, 1618)
  solve(13, 7999)
  solve(17, 1104)
  solve(21, 6111)
  solve(30, 5807)
  solve(452, 70784)
  //  solve(452, 7078400) - it takes hours and may overflow maxint :-(

}


object Day9Part2 extends App {

  case class Marble(var left: Marble, var right: Marble, value: Int) {
    override def toString: String = s"[ ${left.value}, $value, ${right.value}] "

    def removeAndRight = {
      left.right = right
      right.left = left
      right
    }

    def insertRight(value: Int) = {
      val newMarble = Marble(this, right, value)
      right.left = newMarble
      right = newMarble
      newMarble
    }

    def left(n: Int): Marble = if (n == 0) this else left.left(n - 1)
  }

  case class Game(highestMarble: Int, highScore: BigDecimal, current: Marble, scores: Array[BigDecimal]) {
    def player = ( ( highestMarble - 1 ) % scores.size ) + 1

    def loopFrom(cell: Marble): Stream[Marble] = cell #:: loopFrom(cell.right)

    def allCells(cell: Marble) = cell +: loopFrom(cell.right).takeWhile(_.value != cell.value).toList

    def show =
      println(s"[${player}] " +
        allCells(current).map(_.value).mkString(" ") +
        s" [$highScore]"
      )

    def play: Game = {
      val marbleNummber = highestMarble + 1
      if (marbleNummber % 23 != 0) {
        Game(marbleNummber, highScore, current.right.insertRight(marbleNummber), scores)
      } else {
        val left7 = current.left(7)
        val player = ( marbleNummber - 1 ) % scores.size
        val newScore = scores(player) + left7.value + marbleNummber
        val newScores = scores.updated(player, newScore)
        val newHighScore = if (newScore > highScore) newScore else highScore

        Game(marbleNummber, newHighScore, left7.removeAndRight, newScores)
      }
    }

  }

  def game0(players: Int) = {
    val cell = new Marble(null, null, 0)
    cell.left = cell
    cell.right = cell
    Game(0, 0, cell, ( 1 to players ).foldLeft(Array[BigDecimal]())((v, _) => v :+ BigDecimal(0)))
  }


  var start = System.currentTimeMillis

  def points(g: Game): Stream[Game] = {
    //    g.show
    g #:: points(g.play)
  }


  def solve(players: Int, lastMarble: Int) =
    println(s"The high score after marble $lastMarble was ${
      points(game0(players)).dropWhile(_.highestMarble < lastMarble).head.highScore
    }")

  solve(9, 40)
  solve(10, 1618)
  solve(13, 7999)
  solve(17, 1104)
  solve(21, 6111)
  solve(30, 5807)
  solve(452, 70784)
  solve(452, 7078400)

}

