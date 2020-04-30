package amo.geometry.problems

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.commands.Command
import amo.geometry.figures.{ Circle, LineLike, Point }
import amo.implicits._

sealed trait Board[T] {
    val points: Set[Point[T]]
    val lines: Set[LineLike[T]]
    val circles: Set[Circle[T]]

    def addPoints(newPoints: Set[Point[T]]): Board[T]
    def addLine(line: LineLike[T]): Board[T]
    def addCircle(circle: Circle[T]): Board[T]
}

object Board{
    def apply[T](
        points: Set[Point[T]] = Set[Point[T]](),
        lines: Set[LineLike[T]] = Set[LineLike[T]](),
        circles: Set[Circle[T]] = Set[Circle[T]]()
    )(
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Board[T] = {
        val intersects = lines.toVector.combinations(2).flatMap({
            case Seq(l1, l2) => l1.intersects(l2)
        }) ++ circles.toVector.combinations(2).flatMap({
            case Seq(c1, c2) => c1.intersects(c2)
        }) ++ (for {
            l <- lines
            c <- circles
            p <- l.intersects(c)
        } yield p)
        SimpleBoard(points ++ intersects, lines, circles)
    }

    case class SimpleBoard[T](
        points: Set[Point[T]] = Set[Point[T]](),
        lines: Set[LineLike[T]] = Set[LineLike[T]](),
        circles: Set[Circle[T]] = Set[Circle[T]](),
    )(
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ) extends Board[T] {
        def addPoints(newPoints: Set[Point[T]]): Board[T] =
            SimpleBoard(points ++ newPoints, lines, circles)

        def addLine(line: LineLike[T]): Board[T] = {
            val intersects =
                if (lines contains line) Iterator.empty
                else lines.flatMap(_.intersects(line)) ++ circles.flatMap(_.intersects(line))
            SimpleBoard(points ++ intersects, lines + line, circles)
        }

        def addCircle(circle: Circle[T]): Board[T] = {
            val intersects =
                if (circles contains circle) Iterator.empty
                else lines.flatMap(_.intersects(circle)) ++ circles.flatMap(_.intersects(circle))
            SimpleBoard(points ++ intersects, lines, circles + circle)
        }
    }
}
