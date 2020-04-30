package amo.geometry.problems

trait ProblemAnswer[T] {
    val cost: Cost

    def fulfill(board: Board[T]): Boolean
}

class FullFillEnvironmentAnswer[T](
    val answerBoard: Board[T],
    val cost: Cost
) extends ProblemAnswer[T] {
    def fulfill(board: Board[T]): Boolean =
        answerBoard.points.subsetOf(board.points) &&
        answerBoard.lines.subsetOf(board.lines) &&
        answerBoard.circles.subsetOf(board.circles)
}
