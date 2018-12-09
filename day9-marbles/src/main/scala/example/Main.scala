package example


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
//        val newMarbles = ( marbles.slice(0, pos + 1) :+ marbleNummber ) ++ marbles.slice(pos + 1, size)
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


  val start = System.currentTimeMillis

  def points(g: Game): Stream[Game] = {
    if (g.highestMarble > 1 && g.highestMarble % 50000 == 0) {
      val speed = ( g.highestMarble * 1000 ) / ( System.currentTimeMillis - start )
      println(s" ${g.highestMarble}, speed = $speed")
    }
    g #:: points(g.play)
  }

  def solve(players: Int, lastMarble: Int) =
    println(s"The high score after marble $lastMarble was ${points(game0(players)).dropWhile(_.highestMarble < lastMarble).head.highScore}")

  solve(10, 1618)
  solve(13, 7999)
  solve(17, 1104)
  solve(21, 6111)
  solve(30, 5807)
  solve(452, 70784)
  solve(452, 7078400)

}

