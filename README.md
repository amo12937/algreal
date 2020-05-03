# Compile

```
sbt compile
```

# Test

```
sbt test
```

# Console

```
sbt console
```

# Examples

## AlgReal

```
scala> import amo.algreal.AlgReal
import amo.algreal.AlgReal

scala> import amo.implicits._
import amo.implicits._

scala> val one = AlgReal(1)
one: amo.algreal.AlgReal = Rat(1)

scala> val half = AlgReal(1, 2)
half: amo.algreal.AlgReal = Rat(1 / 2)

scala> val sqrt2 = AlgReal(2).sqrt
sqrt2: amo.algreal.AlgReal = AlgRealPoly(x^2 - 2, (3 / 4, 3 / 2))
```

`AlgRealPoly(f, (lb, ub))` represents one of the roots of a polynomial `f` specified by interval between `lb` and `ub`.

```
scala> val sqrt3 = AlgReal(3).sqrt
sqrt3: amo.algreal.AlgReal = AlgRealPoly(x^2 - 3, (1, 2))

scala> ((sqrt2 + sqrt3)^2) - 2 * sqrt2 * sqrt3
res0: amo.algreal.AlgReal = Rat(5)
```


## Unipoly

```
scala> import amo.algreal.polynomial.Unipoly
import amo.algreal.polynomial.Unipoly

scala> import amo.algreal.AlgReal
import amo.algreal.AlgReal

scala> import amo.implicits._
import amo.implicits._

scala> val x = Unipoly.ind[BigInt]
x: amo.algreal.polynomial.Unipoly[BigInt] = [0, 1]

scala> val f = (x^6) - (x^5) - 2 * (x^4) - (x^3)  + 3 * (x^2) + 6 * x - 6
f: amo.algreal.polynomial.Unipoly[BigInt] = [-6, 6, 3, -1, -2, -1, 1]

scala> val fs = AlgReal.realRoots(f).toVector
fs: Vector[amo.algreal.AlgReal] = Vector(Rat(1), AlgRealPoly(x^2 - 2, (-3 / 2, -3 / 4)), AlgRealPoly(x^2 - 2, (3 / 4, 3 / 2)), AlgRealPoly(x^3 - 3, (1, 2)))

scala> fs.foreach(println(_))
Rat(1)
AlgRealPoly(x^2 - 2, (-3 / 2, -3 / 4))
AlgRealPoly(x^2 - 2, (3 / 4, 3 / 2))
AlgRealPoly(x^3 - 3, (1, 2))
```

## Euclidea

See also: https://www.4gamer.net/games/467/G046758/20190628121/

```
scala> import amo.implicits._
import amo.implicits._

scala> import amo.geometry.problems.solvers._
import amo.geometry.problems.solvers._

scala> import amo.euclidea.alpha._
import amo.euclidea.alpha._

scala> import amo.algreal.AlgReal
import amo.algreal.AlgReal

scala> val solver = new BreadthFirstSearchSolver[AlgReal]
solver: amo.geometry.problems.solvers.BreadthFirstSearchSolver[amo.algreal.AlgReal] = amo.geometry.problems.solvers.BreadthFirstSearchSolver@6e270027

scala> solver.solve(Alpha_1_2_VerticalBisector.problem.problemL).foreach(println(_))
Draw a circle passing through Point(Rat(2),Rat(0)) with the center Point(Rat(0),Rat(0))
Draw a circle passing through Point(Rat(0),Rat(0)) with the center Point(Rat(2),Rat(0))
Draw a line connecting point Point(Rat(1),AlgRealPoly(x^2 - 3, (13 / 8, 13 / 4))) and point Point(Rat(1),AlgRealPoly(x^2 - 3, (-13 / 4, -13 / 8)))
```

Calculating `AlgReal` number needs high computational resources and even alpha 1.5 (finding a rhombus in rectangle) never stop in a few hours.

```
scala> solver.solve(Alpha_1_5_RhombusInRectangle.problem.problemL).foreach(println(_))

... (Hang in there, CPU)

```

Instead of using `AlgReal`, There's `DoubleWrapper` which is a double value with allowable error.

```
scala> import amo.algreal.field.DoubleAlgebraicClosure.implicits._
import amo.algreal.field.DoubleAlgebraicClosure.implicits._

scala> import amo.algreal.field.DoubleWrapper
import amo.algreal.field.DoubleWrapper

scala> import amo.geometry.problems.solvers._
import amo.geometry.problems.solvers._

scala> import amo.euclidea.alpha._
import amo.euclidea.alpha._

scala> val solver = new BreadthFirstSearchSolver[DoubleWrapper]
solver: amo.geometry.problems.solvers.BreadthFirstSearchSolver[amo.algreal.field.DoubleWrapper] = amo.geometry.problems.solvers.BreadthFirstSearchSolver@2aeb88cb

scala> solver.solve(Alpha_1_5_RhombusInRectangle.problem.problemL).foreach(println(_))
Draw a vertical bisector between point Point(0.0,0.0) and Point(3.0,1.0)
Draw a line connecting point Point(0.0,0.0) and point Point(1.3333333333333335,1.0)
Draw a line connecting point Point(1.6666666666666667,-0.0) and point Point(3.0,1.0)
```

