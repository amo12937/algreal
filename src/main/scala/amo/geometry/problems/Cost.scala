package amo.geometry.problems

case class Cost(costL: Int, costE: Int) {
    def + (rhs: Cost) = Cost(costL + rhs.costL, costE + rhs.costE)
}
