package amo.AlgReal

trait EuclideanDomainTrait[T] extends GcdDomainTrait[T] {
    def divMod(a: T, b: T): (T, T)
    def mod(a: T, b: T): T

    def exgcd(a: T, b: T): (T, T, T) = {
        def tailRec(
            x: T, y: T, s0: T, s1: T, t0: T, t1: T
        ): (T, T, T) = if (equiv(y, zero)) (x, s0, t0) else {
            val (q, r) = divMod(x, y)
            tailRec(
                y, r,
                s1, sub(s0, times(q, s1)),
                t1, sub(t0, times(q, t1))
            )
        }
        tailRec(a, b, one, zero, zero, one)
    }

    def inverseMod(x: T, m: T): T = {
        val (s, t, _) = exgcd(x, m)
        val mOne = negate(one)
        if (equiv(s, one) || equiv(s, mOne)) times(s, t)
        else throw new IllegalArgumentException(s"${x} has no inverse modulo ${m}")
    }

    /**
     * calculate a^n mod m
     */
    def powMod(aa: T, nn: Int, m: T): T = {
        def tailRec(n: Int, acc: T, a: T): T = if (n == 0) acc else {
            val q = n / 2
            val r = n % 2
            tailRec(
                q,
                if (r == 0) acc else mod(times(acc, a), m),
                mod(times(a, a), m)
            )
        }
        tailRec(nn, one, mod(aa, m))
    }
}
