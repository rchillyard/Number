package com.phasmidsoftware.number.java;

import com.phasmidsoftware.number.core.Number;
import com.phasmidsoftware.number.core.*;
import org.junit.Test;

import static org.junit.Assert.*;

public class ExpressionTest {

    @Test
    public void literalTest() {
        Expression piBy2 = Literal.apply(Number.piBy2());
        assertTrue(piBy2.isExact());
        assertTrue(piBy2.isAtomic());
        assertEquals(com.phasmidsoftware.number.core.Constants.piBy2(), piBy2.materialize());
    }

    @Test
    public void addTest() {
        Expression piBy2 = Literal.apply(Number.piBy2());
        Expression pi = ExpressionJ.add(piBy2, Literal.apply(2));
        assertFalse(pi.isExact());
        assertFalse(pi.isAtomic());
        assertEquals(0, Real.apply("3.5707963267949*").compareTo(pi.materialize()));
    }

    @Test
    public void multiplyTest() {
        Expression piBy2 = Literal.apply(Number.piBy2());
        Expression pi = ExpressionJ.multiply(piBy2, Literal.apply(2));
        assertTrue(pi.isExact());
        assertFalse(pi.isAtomic());
        assertEquals(Constants.pi(), pi.materialize());
    }
}