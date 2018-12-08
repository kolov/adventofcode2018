package example

import scala.io.Source
import Shared._


object Day8Part1 extends App {
  val chars: Stream[Char] = Source.fromResource("input-part1.txt").getLines.toList.reduce(_ + _).toStream

  def readNode(ints: Stream[Int]): (Int, Stream[Int]) = {
    val countNodes = ints.head
    val countMeta = ints.tail.head
    println(s"reading node $countNodes $countMeta")
    val (sumOtherMetas, tail) = ( 0 to countNodes - 1 ).foldLeft((0, ints.drop(2))) {
      case ((sum: Int, tail: Stream[Int]), _) =>
        val (sumOtherMetas, t) = readNode(tail)
        (sum + sumOtherMetas, t)
    }
    val sumThisMeta = tail.take(countMeta).foldLeft(0)(_ + _)
    (sumOtherMetas + sumThisMeta, tail.drop(countMeta))
  }

  val (sum, _) = readNode(ints(chars))
  println(s"Total: $sum")
}

object Day8Part2 extends App {
  val chars: Stream[Char] = Source.fromResource("input-part2.txt").getLines.toList.reduce(_ + _).toStream

  case class Node(childrenCount: Int, metaCount: Int, children: List[Node], meta: List[Int]) {
    def valueOf(ix: Int): Int = if (ix < childrenCount + 1) children(ix - 1).value else 0

    def value: Int =
      if (children.isEmpty)
        meta.foldLeft(0)(_ + _)
      else
        meta.foldLeft(0) { case (acc, ix) => acc + valueOf(ix) }
  }

  def readNode(ints: Stream[Int]): (Node, Stream[Int]) = {
    val countNodes = ints.head
    val countMeta = ints.tail.head
    println(s"reading node $countNodes $countMeta")
    val (children, tail) = ( 0 to countNodes - 1 ).foldLeft((List[Node](), ints.drop(2))) {
      case ((l, tail), _) =>
        val (node, t) = readNode(tail)
        (l :+ node, t)
    }
    (Node(countNodes, countMeta, children, tail.take(countMeta).toList), tail.drop(countMeta))
  }

  val (node, _) = readNode(ints(chars))
  println(s"Total: ${node.value}")
}

object Shared {
  def ints(chars: Stream[Char]): Stream[Int] = {
    val tail = chars.dropWhile(!_.isDigit)
    val digits = tail.takeWhile(_.isDigit).toList.mkString("")
    if (digits.length > 0)
      digits.toInt #:: ints(tail.drop(digits.length))
    else Stream.empty
  }
}
