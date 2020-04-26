package amo.algreal

class Resultant[T](implicit integralDomain: IntegralDomainTrait[T]) {
    def resultant(f: Unipoly[T], g: Unipoly[T]): T = {
        lazy val r = f.pseudoMod(g)
        lazy val sign = 1 - 2 * ((f.degreeInt * g.degreeInt) % 2)
        lazy val lcg = g.leadingCoefficient
        lazy val l = f.degreeInt - r.degreeInt - (f.degreeInt - g.degreeInt + 1) * g.degreeInt
        if ((f.isZero && g.degreeInt == 0) || (f.degreeInt == 0 && g.isZero)) integralDomain.one
        else if (f.isZero || g.isZero) integralDomain.zero
        else if (f.degreeInt == 0) integralDomain.pow(f.leadingCoefficient, g.degreeInt)
        else if (g.degreeInt == 0) integralDomain.pow(g.leadingCoefficient, f.degreeInt)
        else if (r.isZero) integralDomain.zero
        else if (f.degreeInt < g.degreeInt) integralDomain.timesN(resultant(g, f), sign)
        else if (l >= 0) integralDomain.times(
            integralDomain.timesN(resultant(g, r), sign),
            integralDomain.pow(lcg, l)
        ) else integralDomain.divide(
            integralDomain.timesN(resultant(g, r), sign),
            integralDomain.pow(lcg, -l)
        )
    }
}
