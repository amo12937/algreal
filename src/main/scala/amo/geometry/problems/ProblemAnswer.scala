package amo.geometry.problems

trait ProblemAnswer[T] {
    val cost: Cost

    def fulfill(board: Board[T]): Boolean
    def remaining(board: Board[T]): Int
}

class FullFillEnvironmentAnswer[T](
    val answerBoards: Vector[Board[T]],
    val cost: Cost
) extends ProblemAnswer[T] {
    def fulfill(board: Board[T]): Boolean =
        answerBoards.exists(ab => {
            ab.points.subsetOf(board.points) &&
            ab.lines.subsetOf(board.lines) &&
            ab.circles.subsetOf(board.circles)
        })

    def remaining(board: Board[T]): Int =
        answerBoards.map(ab => {
            (ab.lines -- board.lines).size + (ab.circles -- board.circles).size
        }).min
}
