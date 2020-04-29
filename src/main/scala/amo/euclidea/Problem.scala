package amo.euclidea

import amo.geometry.problems.{
    ProblemL => GeometryProblemL,
    ProblemE => GeometryProblemE
}

trait Problem[T] {
    val problemL: GeometryProblemL[T]
    val problemE: GeometryProblemE[T]
}
