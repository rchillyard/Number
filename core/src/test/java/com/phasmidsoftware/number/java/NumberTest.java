package com.phasmidsoftware.number.java;

import com.phasmidsoftware.number.core.numerical.Number;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.Assert.assertEquals;

public class NumberTest {

    @Test
    public void bigIntegerToNumber() {
        BigInteger bigInteger = BigInteger.valueOf(Long.MAX_VALUE);
        Number x = NumberJ.bigIntegerToNumber(bigInteger);
        assertEquals(bigInteger, NumberJ.numberToBigInteger(x));
    }

    @Test
    public void stringToNumber1() {
        String pi = "22/7";
        Number x = NumberJ.stringToNumber(pi);
        assertEquals("3.<142857>", x.toString());
    }

    @Test
    public void stringToNumber2() {
        String pi = "3.1415927";
        Number x = NumberJ.stringToNumber(pi);
        assertEquals("3.1415927", x.toString());
    }

    @Test
    public void longToNumber() {
        Long maxLong = Long.MAX_VALUE;
        Number x = NumberJ.longToNumber(maxLong);
        assertEquals(maxLong, NumberJ.numberToLong(x));
    }

    @Test
    public void doubleToNumber() {
        double pi = Math.PI;
        Number x = NumberJ.doubleToNumber(pi);
        assertEquals(pi, NumberJ.numberToDouble(x), 1E-15);
    }

    @Test
    public void bigDecimalToNumber() {
        BigDecimal bigDecimal = BigDecimal.valueOf(Long.MAX_VALUE);
        Number x = NumberJ.bigDecimalToNumber(bigDecimal);
        assertEquals(bigDecimal, NumberJ.numberToBigDecimal(x));
    }
}