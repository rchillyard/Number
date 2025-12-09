package com.phasmidsoftware.number.java;

import com.phasmidsoftware.number.core.numerical.Complex;
import com.phasmidsoftware.number.core.numerical.ComplexCartesian;
import com.phasmidsoftware.number.core.numerical.ComplexPolar;
import com.phasmidsoftware.number.core.numerical.Number;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ComplexTest {

    @Test
    public void conversionTest() {
        Complex complexCartesian = Complex.convertToCartesian(new ComplexPolar(Number.one(), Number.piBy2(), 1));
        assertEquals(new ComplexCartesian(Number.zero(), Number.one()), complexCartesian);
    }
}