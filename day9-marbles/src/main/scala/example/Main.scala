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
  }

  case class Game(highestMarble: Int, highScore: BigDecimal, current: Marble, scores: Array[BigDecimal]) {
    def player = ( ( highestMarble - 1 ) % scores.size ) + 1

    def loopFrom(cell: Marble): Stream[Marble] = cell #:: loopFrom(cell.right)

    def allCells(cell: Marble) = cell +: loopFrom(cell.right).takeWhile(_.value != cell.value).toList

    def show =
      println(s"[${player + 1}] " +
        allCells(current).mkString(" ") +
        s" [$highScore]"
      )

    def play: Game = {
      val marbleNummber = highestMarble + 1
      if (marbleNummber % 23 != 0) {
        val right1 = current.right
        val right2 = right1.right
        val newCell = Marble(right1, right2, marbleNummber)
        right1.right = newCell
        right2.left = newCell

        Game(marbleNummber, highScore, newCell, scores)
      } else {
        val left6 = current.left.left.left.left.left.left
        val left7 = left6.left
        val left8 = left7.left
        left8.right = left6
        left6.left = left8
        val player = ( marbleNummber - 1 ) % scores.size
        val newScore = scores(player) + left7.value + marbleNummber
        val newScores = scores.updated(player, newScore)
        val newHighScore = if (newScore > highScore) newScore else highScore

        Game(marbleNummber, newHighScore, left6, newScores)
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

  def points(g: Game): Stream[Game] = g #:: points(g.play)


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

