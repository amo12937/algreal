package amo.geometry.problems

trait ProblemAnswer[T] {
    val cost: Cost

    def fulfill(problemEnvironment: ProblemEnvironment[T]): Boolean
}

class FullFillEnvironmentAnswer[T](
    val cost: Cost,
    val answerEnvironment: ProblemEnvironment[T]
) extends ProblemAnswer[T] {
    def fulfill(problemEnvironment: ProblemEnvironment[T]): Boolean =
        answerEnvironment.points.subsetOf(problemEnvironment.points) &&
        answerEnvironment.lines.subsetOf(problemEnvironment.lines) &&
        answerEnvironment.circles.subsetOf(problemEnvironment.circles)
}
