package amo.geometry.problems

trait ProblemAnswer[T] {
    val costL: Int
    val costE: Int

    def fulfill(problemEnvironment: ProblemEnvironment[T]): Boolean
}

class FullFillEnvironmentAnswer[T](
    answerEnvironment: ProblemEnvironment[T]
) extends ProblemAnswer[T] {
    val costL = answerEnvironment.costL
    val costE = answerEnvironment.costE

    def fulfill(problemEnvironment: ProblemEnvironment[T]): Boolean =
        answerEnvironment.points.subsetOf(problemEnvironment.points) &&
        answerEnvironment.lines.subsetOf(problemEnvironment.lines) &&
        answerEnvironment.circles.subsetOf(problemEnvironment.circles)
}
