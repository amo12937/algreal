package amo.geometry.problems.solvers

import scala.language.higherKinds

import amo.geometry.commands.Command
import amo.geometry.problems.Problem

trait Solver[T, F[_]] {
    def solve(problem: Problem[T]): F[Vector[Command[T]]];
}
