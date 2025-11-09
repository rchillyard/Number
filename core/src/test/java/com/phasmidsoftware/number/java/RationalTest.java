package com.phasmidsoftware.number.java;

import com.phasmidsoftware.number.core.inner.Rational;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.Assert.assertEquals;

public class RationalTest {

    @Test
    public void bigIntegerToRational() {
        BigInteger bigInteger = BigInteger.valueOf(Long.MAX_VALUE);
        Rational r = RationalJ.bigIntegerToRational(bigInteger);
        assertEquals(bigInteger, RationalJ.rationalToBigInteger(r));
    }

    @Test
    public void stringToRational1() {
        String pi = "22/7";
        Rational r = RationalJ.stringToRational(pi);
        assertEquals(pi, r.toRationalString()); // NOTE the use of toRationalString
    }

    @Test
    public void stringToRational2() {
        String pi = "3.1415927";
        Rational r = RationalJ.stringToRational(pi);
        assertEquals(pi, r.render());
    }

    @Test
    public void longToRational() {
        Long maxLong = Long.MAX_VALUE;
        Rational r = RationalJ.longToRational(maxLong);
        assertEquals(maxLong, RationalJ.rationalToLong(r));
    }

    @Test
    public void doubleToRational() {
        double pi = Math.PI;
        Rational r = RationalJ.doubleToRational(pi);
        assertEquals(pi, RationalJ.rationalToDouble(r), 1E-15);
    }

    @Test
    public void bigDecimalToRational() {
        BigDecimal bigDecimal = BigDecimal.valueOf(Long.MAX_VALUE);
        Rational r = RationalJ.bigDecimalToRational(bigDecimal);
        assertEquals(bigDecimal, RationalJ.rationalToBigDecimal(r));
    }
}