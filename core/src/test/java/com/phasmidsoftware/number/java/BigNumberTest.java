package com.phasmidsoftware.number.java;

import com.phasmidsoftware.number.core.inner.Rational;
import com.phasmidsoftware.number.core.numerical.BigNumber;
import org.junit.Test;
import scala.util.Try;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.Assert.*;

public class BigNumberTest {
    // NOTE that this approximatePi is nevertheless an exact number. It's an exact number that is close to pi. But we know it is not pi.
    final BigNumber approximatePi = BigNumber.value(3, 1415927, true);

    @Test
    public void testBigNumberPrimary() {
        assertEquals(new BigNumber(BigInteger.ONE, new int[]{}, true), new BigNumber(BigInteger.ONE, new int[]{0, 0, 0}, true));
    }

    @Test
    public void testBigNumber() {
        assertEquals(new BigNumber(BigInteger.TEN, new int[]{}, false), new BigNumber(-10));
    }

    @Test
    public void testIntValue() {
        assertEquals(-1, BigNumber.value(-1).intValue());
        assertEquals(0, BigNumber.value(0).intValue());
        assertEquals(1, BigNumber.value(1).intValue());
    }

    @Test(expected = ArithmeticException.class)
    public void testIntValueBad1() {
        assertEquals(0, BigNumber.value(Long.MAX_VALUE).intValue());
    }

    @Test(expected = Exception.class)
    public void testIntValueBad2() {
        assertEquals(0, BigNumber.pi.longValue());
    }

    @Test
    public void testLongValue() {
        assertEquals(0L, BigNumber.value(0).longValue());
    }

    @Test(expected = ArithmeticException.class)
    public void testLongValueBad1() {
        assertEquals(0L, BigNumber.value(Long.MAX_VALUE).add(BigNumber.one).longValue());
    }

    @Test(expected = Exception.class)
    public void testLongValueBad2() {
        assertEquals(0L, BigNumber.pi.longValue());
    }

    @Test
    public void testDoubleValue() {
        assertEquals(-1, BigNumber.one.negate().doubleValue(), 1E15);
        assertEquals(0, BigNumber.zero.doubleValue(), 1E15);
        assertEquals(1, BigNumber.one.doubleValue(), 1E15);
        assertEquals(Math.PI, BigNumber.pi.doubleValue(), 1E15);
    }

    @Test
    public void testFloatValue() {
        assertEquals(0., BigNumber.value(0).floatValue(), 1E-5);
    }

    @Test
    public void testValue() {
        assertEquals(new BigNumber(BigInteger.ZERO, new int[]{}, true), BigNumber.value(0, 0, true));
        assertEquals(new BigNumber(BigInteger.valueOf(3), new int[]{1, 4, 1, 5, 9, 2, 7}, true), BigNumber.value(3, 1415927, true));
    }

    @Test
    public void testParse() {
        assertEquals(BigNumber.zero, BigNumber.parse("0"));
        assertEquals(new BigNumber(BigInteger.valueOf(3), new int[]{1, 4, 1, 5, 9, 2, 7}, true), BigNumber.parse("3.1415927"));
    }

    @Test
    public void testIsExact() {
        assertTrue(BigNumber.zero.isExact());
        assertTrue(BigNumber.parse("0").isExact());
        assertTrue(new BigNumber(BigInteger.valueOf(3), new int[]{1, 4, 1, 5, 9, 2, 7}, true).isExact());
        assertTrue(BigNumber.parse("3.1415927").isExact());
        assertFalse(BigNumber.one.divide(3).isExact());
        assertFalse(BigNumber.value(Rational.apply("1/3")).isExact());

    }

    @Test
    public void testAdd1() {
        BigNumber target = BigNumber.value(3, 1415927);
        BigNumber addend = BigNumber.value(1, 111111);
        assertEquals(new BigNumber(BigInteger.valueOf(4), new int[]{2, 5, 2, 7, 0, 3, 7}, true), target.add(addend));
    }

    @Test
    public void testAdd2() {
        BigNumber addend = BigNumber.value(1, 111111, false);
        assertEquals(new BigNumber(BigInteger.TWO, new int[]{0, 3, 0, 4, 8, 1, 7}, true), approximatePi.add(addend));
    }

    @Test
    public void testAdd3() {
        BigNumber target = BigNumber.value(3, 1415927, false);
        BigNumber addend = BigNumber.value(1, 111111, false);
        assertEquals(new BigNumber(BigInteger.valueOf(4), new int[]{2, 5, 2, 7, 0, 3, 7}, false), target.add(addend));
    }

    @Test
    public void testAdd4() {
        BigNumber target = BigNumber.value(3, 1415927, false);
        BigNumber addend = BigNumber.value(1, 111111);
        assertEquals(new BigNumber(BigInteger.TWO, new int[]{0, 3, 0, 4, 8, 1, 7}, false), target.add(addend));
    }

    @Test
    public void testAddWithDecimal2() {
        BigNumber addend = BigNumber.value(0, 240, true);
        BigNumber target = BigNumber.value(0, 20, false);
        assertEquals("0.04", target.add(addend).toString());
    }

    @Test
    public void testAddDecimal3() {
        BigNumber addend = BigNumber.value(200, 5234, false);
        BigNumber target = BigNumber.value(100, 5000, true);
        assertEquals("-100.0234", target.add(addend).toString());
    }

    @Test
    public void testAddDecimal4() {
        BigNumber addend = BigNumber.value(100, 5234, false);
        BigNumber target = BigNumber.value(235, 18924, true);
        assertEquals("134.66584", target.add(addend).toString());
    }

    @Test
    public void testAddDecimal5() {
        BigNumber addend = new BigNumber(BigInteger.ZERO, new int[]{1, 7, 6, 4}, true);
        BigNumber target = new BigNumber(BigInteger.ZERO, new int[]{0, 9}, false);
        assertEquals(new BigNumber(BigInteger.ZERO, new int[]{0, 8, 6, 4}, true), target.add(addend));
    }

    @Test
    public void testAddDecimal6() {
        BigNumber addend = BigNumber.value(200, 5234, false);
        BigNumber target = BigNumber.value(100, 5005, false);
        assertEquals("-301.0239", target.add(addend).toString());
    }

    @Test
    public void testAddDecimal7() {
        BigNumber addend = BigNumber.value(0, 5234, false);
        BigNumber target = BigNumber.value(0, 5005, true);
        assertEquals("-0.0229", target.add(addend).toString());
    }

    @Test
    public void testNegate() {
        assertEquals(new BigNumber(BigInteger.valueOf(3), new int[]{1, 4, 1, 5, 9, 2, 7}, false), BigNumber.value(3, 1415927).negate());
    }

    @Test
    public void testMultiply1() {
        assertEquals(BigNumber.value(3, 1415927, true), BigNumber.value(3, 1415927).multiply(BigNumber.one));
        assertEquals(BigNumber.value(3, 1415927, false), BigNumber.value(3, 1415927).multiply(BigNumber.one.negate()));
        assertEquals(BigNumber.value(3, 1415927, false), BigNumber.value(3, 1415927, false).multiply(BigNumber.one));
        assertEquals(BigNumber.value(3, 1415927, true), BigNumber.value(3, 1415927, false).multiply(BigNumber.one.negate()));
    }

    @Test
    public void testMultiply2() {
        assertEquals(BigNumber.value(1, 21, true), BigNumber.value(1, 1, true).multiply(BigNumber.value(1, 1, true)));
        assertEquals(BigNumber.parse("9.86960469269329"), BigNumber.value(3, 1415927, true).multiply(BigNumber.value(3, 1415927, true)));
    }

    @Test
    public void testMultiply3() {
        BigNumber piSquared = BigNumber.pi.multiply(BigNumber.pi);
        assertEquals("9.869604401089358618834490999876151135313699407240790626413349376220044822419205243001773403718552225653007487119222379933851745525268070040321103742979861060062160525374811732041221722760637532682489", piSquared.toString());
    }

    @Test
    public void testMultiply4() {
        BigNumber ePi = BigNumber.e.multiply(BigNumber.pi);
        assertEquals("8.53973422267356706546355086954657449503488853576508", ePi.toString().substring(0, 52));
    }

    @Test
    public void testDivideLong1() {
        BigNumber target = BigNumber.value(3, 1415927, true);
        assertEquals(BigNumber.value(1L, 57079635L, true), target.divide(2));
        assertEquals(BigNumber.value(0L, 157079635L, true), target.divide(20));
    }

    @Test
    public void testDivideLong2() {
        BigNumber target = BigNumber.value(1L);
        BigNumber oneThird = target.divide(3);

        String string = oneThird.toString();
        assertEquals(1002, string.length());
        assertTrue(string.startsWith("0.3333333333333333"));
    }

    @Test
    public void testDivideLong3() {
        BigNumber target = BigNumber.value(-1L);
        BigNumber oneThird = target.divide(3);

        String string = oneThird.toString();
        assertEquals(1003, string.length());
        assertTrue(string.startsWith("-0.3333333333333333"));
    }

    @Test
    public void testDivideBigInteger() {
        BigNumber target = BigNumber.value(3, 1415927, true);
        assertEquals(BigNumber.value(1L, 57079635L, false), target.divide(BigNumber.value(-2)));
    }

    @Test
    public void testDivideBigNumber1() {
        BigNumber target = BigNumber.value(3, 1415927, true);
        assertEquals(BigNumber.value(1L, 57079635L, true), target.divide(BigNumber.value(2)));
        assertEquals(BigNumber.value(0L, 157079635L, true), target.divide(BigNumber.value(20)));
    }

    @Test
    public void testDivideBigNumber4() {
        BigNumber target = BigNumber.value(3, 1415927, false);
        assertEquals(BigNumber.value(1L, 57079635L, false), target.divide(BigNumber.value(2)));
        assertEquals(BigNumber.value(0L, 157079635L, false), target.divide(BigNumber.value(20)));
    }

    //    @Test
    public void testPrimeReciprocal() {
        BigNumber target = BigNumber.one.divide(17L);
//        BigNumber target = BigNumber.one.divide(524287L);
        assertEquals("", target.toString());
    }

    @Test
    public void testDivideBigNumber2() {
        BigNumber target = BigNumber.value(3, 333333, true);
        BigNumber divisor = BigNumber.value(1, 111111, true);
        assertEquals(BigNumber.value(3L), target.divide(divisor));
    }

    @Test
    public void testAsBigDecimal() {
        BigNumber target = BigNumber.value(3, 1415927, true);
        assertEquals(BigDecimal.valueOf(3.1415927), target.toBigDecimal());
        assertEquals(BigDecimal.valueOf(3.1415927), target.negate().toBigDecimal().negate());
    }

    @Test
    public void testValueOfBigDecimal() {
        BigDecimal value = BigDecimal.valueOf(3.1415927);
        BigNumber target = BigNumber.value(value);
        assertEquals(BigNumber.value(3, 1415927, true), target);
    }

    @Test
    public void testValueOfRational() {
        Try<Rational> triedValue = Rational.parse("3.1415927");
        Rational rational = triedValue.get();
        BigNumber target = BigNumber.value(rational);
        assertEquals(BigNumber.value(3, 1415927, true), target);
    }

    @Test(expected = Exception.class)
    public void testValueOfDouble() {
        // We do not support the (double) value method because doubles are not exact.
        BigNumber.value(Math.PI);
    }

    @Test
    public void testToString() {
        assertEquals("-1", new BigNumber(-1).toString());
        assertEquals("0", new BigNumber(0).toString());
        assertEquals("3", new BigNumber(3).toString());
        assertEquals("3.1", BigNumber.value(3, 1, true).toString());
        assertEquals("3.1415927", BigNumber.value(3, 1415927, true).toString());
    }

    @Test
    public void testKaratsuba1() {
        BigNumber ePi = BigNumber.e.multiplyWithKaratsuba(BigNumber.pi);
        assertEquals("8.53973422267356706546355086954657449503488853576508", ePi.toString().substring(0, 52));
    }

    @Test
    public void testKaratsuba2() {
        BigNumber eSquared = BigNumber.e.multiplyWithKaratsuba(BigNumber.e);
        assertEquals("7.389056098930650227230427", eSquared.toString().substring(0, 26));
    }

    @Test
    public void testDoMain() {
        BigNumber.doMain();
    }

    /**
     * The following tests are by Claude.
     */

    @Test
    public void testConstructorWithLong() {
        BigNumber bn = new BigNumber(42);
        assertEquals("42", bn.toString());
        assertTrue(bn.isWhole());
        assertTrue(bn.isExact());
    }

    @Test
    public void testConstructorWithNegativeLong() {
        BigNumber bn = new BigNumber(-42);
        assertEquals("-42", bn.toString());
        assertTrue(bn.isWhole());
    }

    @Test
    public void testConstructorWithBigInteger() {
        BigInteger bi = new BigInteger("123456789012345678901234567890");
        BigNumber bn = new BigNumber(bi);
        assertEquals("123456789012345678901234567890", bn.toString());
        assertTrue(bn.isWhole());
    }

    @Test
    public void testConstructorWithNegativeBigInteger() {
        BigInteger bi = new BigInteger("-999");
        BigNumber bn = new BigNumber(bi);
        assertEquals("-999", bn.toString());
    }

    @Test
    public void testConstructorWithWholeAndDecimals() {
        int[] decimals = {1, 4};
        BigNumber bn = new BigNumber(BigInteger.valueOf(3), decimals, true);
        assertEquals("3.14", bn.toString());
    }

    // ========== Factory Method Tests ==========

    @Test
    public void testValueLong() {
        BigNumber bn = BigNumber.value(100L);
        assertEquals("100", bn.toString());
    }

    @Test
    public void testValueBigInteger() {
        BigInteger bi = new BigInteger("987654321");
        BigNumber bn = BigNumber.value(bi);
        assertEquals("987654321", bn.toString());
    }

    @Test
    public void testValueBigDecimal() {
        BigDecimal bd = new BigDecimal("3.14159");
        BigNumber bn = BigNumber.value(bd);
        assertEquals("3.14159", bn.toString());
    }

    @Test
    public void testValueWholeAndDecimals() {
        BigNumber bn = BigNumber.value(3, 14);
        assertEquals("3.14", bn.toString());
    }

    @Test
    public void testValueWholeAndDecimalsWithSign() {
        BigNumber bn = BigNumber.value(3, 14, false);
        assertEquals("-3.14", bn.toString());
    }

    // ========== Parse Tests ==========

    @Test
    public void testParseWholeNumber() {
        BigNumber bn = BigNumber.parse("42");
        assertEquals("42", bn.toString());
        assertTrue(bn.isWhole());
    }

    @Test
    public void testParseNegativeWholeNumber() {
        BigNumber bn = BigNumber.parse("-42");
        assertEquals("-42", bn.toString());
    }

    @Test
    public void testParseDecimalNumber() {
        BigNumber bn = BigNumber.parse("3.14159");
        assertEquals("3.14159", bn.toString());
        assertFalse(bn.isWhole());
    }

    @Test
    public void testParseNegativeDecimalNumber() {
        BigNumber bn = BigNumber.parse("-2.71828");
        assertEquals("-2.71828", bn.toString());
    }

    // Ignored test
    public void testParseLargeNumber() {
        String large = "123456789012345678901234567890.987654321";
        BigNumber bn = BigNumber.parse(large);
        assertEquals(large, bn.toString());
    }

    @Test
    public void testParseZero() {
        BigNumber bn = BigNumber.parse("0");
        assertEquals("0", bn.toString());
        assertTrue(bn.isZero());
    }

    // ========== Constants Tests ==========

    @Test
    public void testZeroConstant() {
        assertTrue(BigNumber.zero.isZero());
        assertEquals("0", BigNumber.zero.toString());
    }

    @Test
    public void testOneConstant() {
        assertFalse(BigNumber.one.isZero());
        assertEquals("1", BigNumber.one.toString());
    }

    @Test
    public void testTwoConstant() {
        assertEquals("2", BigNumber.two.toString());
    }

    @Test
    public void testTenConstant() {
        assertEquals("10", BigNumber.ten.toString());
    }

    @Test
    public void testPiConstant() {
        BigNumber pi = BigNumber.pi;
        String piStr = pi.toString();
        assertTrue(piStr.startsWith("3.14159"));
        assertTrue(pi.isExact()); // pi is not exact
    }

    @Test
    public void testEConstant() {
        String eStr = BigNumber.e.toString();
        assertTrue(eStr.startsWith("2.71828"));
    }

    // ========== Addition Tests ==========

    @Test
    public void testAddWholeNumbers() {
        BigNumber a = new BigNumber(5);
        BigNumber b = new BigNumber(7);
        BigNumber result = a.add(b);
        assertEquals("12", result.toString());
    }

    @Test
    public void testAddNegativeNumbers() {
        BigNumber a = new BigNumber(-5);
        BigNumber b = new BigNumber(-3);
        BigNumber result = a.add(b);
        assertEquals("-8", result.toString());
    }

    @Test
    public void testAddMixedSigns() {
        BigNumber a = new BigNumber(10);
        BigNumber b = new BigNumber(-3);
        BigNumber result = a.add(b);
        assertEquals("7", result.toString());
    }

    @Test
    public void testAddDecimalNumbers() {
        BigNumber a = BigNumber.parse("3.14");
        BigNumber b = BigNumber.parse("2.86");
        BigNumber result = a.add(b);
        assertEquals("6", result.toString());
    }

    @Test
    public void testAddWithDifferentDecimalLengths() {
        BigNumber a = BigNumber.parse("1.5");
        BigNumber b = BigNumber.parse("2.25");
        BigNumber result = a.add(b);
        assertEquals("3.75", result.toString());
    }

    @Test
    public void testAddZero() {
        BigNumber a = new BigNumber(42);
        BigNumber result = a.add(BigNumber.zero);
        assertEquals("42", result.toString());
    }

    // ========== Subtraction Tests ==========

    @Test
    public void testSubtractWholeNumbers() {
        BigNumber a = new BigNumber(10);
        BigNumber b = new BigNumber(3);
        BigNumber result = a.subtract(b);
        assertEquals("7", result.toString());
    }

    @Test
    public void testSubtractResultingInNegative() {
        BigNumber a = new BigNumber(3);
        BigNumber b = new BigNumber(10);
        BigNumber result = a.subtract(b);
        assertEquals("-7", result.toString());
    }

    @Test
    public void testSubtractDecimalNumbers() {
        BigNumber a = BigNumber.parse("5.75");
        BigNumber b = BigNumber.parse("2.5");
        BigNumber result = a.subtract(b);
        assertEquals("3.25", result.toString());
    }

    @Test
    public void testSubtractFromZero() {
        BigNumber a = BigNumber.zero;
        BigNumber b = new BigNumber(5);
        BigNumber result = a.subtract(b);
        assertEquals("-5", result.toString());
    }

    // ========== Multiplication Tests ==========

    @Test
    public void testMultiplyWholeNumbers() {
        BigNumber a = new BigNumber(6);
        BigNumber b = new BigNumber(7);
        BigNumber result = a.multiply(b);
        assertEquals("42", result.toString());
    }

    @Test
    public void testMultiplyByZero() {
        BigNumber a = new BigNumber(42);
        BigNumber result = a.multiply(BigNumber.zero);
        assertEquals("0", result.toString());
    }

    @Test
    public void testMultiplyByOne() {
        BigNumber a = new BigNumber(42);
        BigNumber result = a.multiply(BigNumber.one);
        assertEquals("42", result.toString());
    }

    @Test
    public void testMultiplyNegativeNumbers() {
        BigNumber a = new BigNumber(-5);
        BigNumber b = new BigNumber(-3);
        BigNumber result = a.multiply(b);
        assertEquals("15", result.toString());
    }

    @Test
    public void testMultiplyMixedSigns() {
        BigNumber a = new BigNumber(5);
        BigNumber b = new BigNumber(-3);
        BigNumber result = a.multiply(b);
        assertEquals("-15", result.toString());
    }

    @Test
    public void testMultiplyDecimalNumbers() {
        BigNumber a = BigNumber.parse("2.5");
        BigNumber b = BigNumber.parse("4");
        BigNumber result = a.multiply(b);
        assertEquals("10", result.toString());
    }

    @Test
    public void testMultiplyDecimalsWithDecimals() {
        BigNumber a = BigNumber.parse("1.5");
        BigNumber b = BigNumber.parse("2.5");
        BigNumber result = a.multiply(b);
        assertEquals("3.75", result.toString());
    }

    // ========== Karatsuba Multiplication Tests ==========

    @Test
    public void testMultiplyWithKaratsubaBasic() {
        BigNumber a = new BigNumber(12);
        BigNumber b = new BigNumber(34);
        BigNumber result = a.multiplyWithKaratsuba(b);
        assertEquals("408", result.toString());
    }

    @Test
    public void testMultiplyWithKaratsubaMatchesStandard() {
        BigNumber a = BigNumber.parse("123.456");
        BigNumber b = BigNumber.parse("789.012");
        BigNumber standard = a.multiply(b);
        BigNumber karatsuba = a.multiplyWithKaratsuba(b);
        assertEquals(standard.toString(), karatsuba.toString());
    }


    // ========== Division Tests ==========

    @Test
    public void testDivideWholeNumbers() {
        BigNumber a = new BigNumber(42);
        BigNumber b = new BigNumber(6);
        BigNumber result = a.divide(b);
        assertEquals("7", result.toString());
        assertTrue(result.isExact());
    }

    @Test
    public void testDivideWithRemainder() {
        BigNumber a = new BigNumber(10);
        BigNumber b = new BigNumber(3);
        BigNumber result = a.divide(b);
        assertTrue(result.toString().startsWith("3.333"));
        assertFalse(result.isExact());
    }

    @Test
    public void testDivideByOne() {
        BigNumber a = new BigNumber(42);
        BigNumber result = a.divide(BigNumber.one);
        assertEquals("42", result.toString());
    }

    @Test
    public void testDivideOneByTwo() {
        BigNumber result = BigNumber.one.divide(BigNumber.two);
        assertEquals("0.5", result.toString());
    }

    @Test
    public void testDivideDecimalNumbers() {
        BigNumber a = BigNumber.parse("7.5");
        BigNumber b = BigNumber.parse("2.5");
        BigNumber result = a.divide(b);
        assertEquals("3", result.toString());
    }

    @Test
    public void testDivideByBigInteger() {
        BigNumber a = new BigNumber(100);
        BigNumber result = a.divide(BigInteger.valueOf(4));
        assertEquals("25", result.toString());
    }

    // ========== Power Tests ==========

    @Test
    public void testPowerPositiveExponent() {
        BigNumber a = new BigNumber(2);
        BigNumber result = a.power(3);
        assertEquals("8", result.toString());
    }

    @Test
    public void testPowerZeroExponent() {
        BigNumber a = new BigNumber(42);
        BigNumber result = a.power(0);
        assertEquals("1", result.toString());
    }

    @Test
    public void testPowerOneExponent() {
        BigNumber a = new BigNumber(42);
        BigNumber result = a.power(1);
        assertEquals("42", result.toString());
    }

    @Test
    public void testPowerNegativeExponent() {
        BigNumber a = new BigNumber(2);
        BigNumber result = a.power(-2);
        assertEquals("0.25", result.toString());
    }

    @Test
    public void testPowerOfZero() {
        BigNumber result = BigNumber.zero.power(5);
        assertEquals("0", result.toString());
    }

    @Test
    public void testPowerOfOne() {
        BigNumber result = BigNumber.one.power(100);
        assertEquals("1", result.toString());
    }

    // ========== Comparison Tests ==========

    @Test
    public void testCompareToEqual() {
        BigNumber a = new BigNumber(42);
        BigNumber b = new BigNumber(42);
        assertEquals(0, a.compareTo(b));
    }

    @Test
    public void testCompareToLess() {
        BigNumber a = new BigNumber(10);
        BigNumber b = new BigNumber(20);
        assertTrue(a.compareTo(b) < 0);
    }

    @Test
    public void testCompareToGreater() {
        BigNumber a = new BigNumber(20);
        BigNumber b = new BigNumber(10);
        assertTrue(a.compareTo(b) > 0);
    }

    @Test
    public void testCompareToWithDecimals() {
        BigNumber a = BigNumber.parse("3.14");
        BigNumber b = BigNumber.parse("3.15");
        assertTrue(a.compareTo(b) < 0);
    }

    @Test
    public void testCompareToNegativeNumbers() {
        BigNumber a = new BigNumber(-5);
        BigNumber b = new BigNumber(-10);
        assertTrue(a.compareTo(b) > 0);
    }

    // ========== Equality Tests ==========

    @Test
    public void testEqualsIdentical() {
        BigNumber a = new BigNumber(42);
        BigNumber b = new BigNumber(42);
        assertEquals(a, b);
    }

    @Test
    public void testEqualsWithDecimals() {
        BigNumber a = BigNumber.parse("3.14");
        BigNumber b = BigNumber.parse("3.14");
        assertEquals(a, b);
    }

    @Test
    public void testNotEquals() {
        BigNumber a = new BigNumber(42);
        BigNumber b = new BigNumber(43);
        assertNotEquals(a, b);
    }

    @Test
    public void testEqualsWithTrailingZeros() {
        BigNumber a = BigNumber.parse("3.14");
        BigNumber b = BigNumber.parse("3.140");
        assertEquals(a, b);
    }

    @Test
    public void testEqualsSameObject() {
        BigNumber a = new BigNumber(42);
        assertEquals(a, a);
    }

    @Test
    public void testEqualsNull() {
        BigNumber a = new BigNumber(42);
        assertNotEquals(null, a);
    }

    // ========== Hash Code Tests ==========

    @Test
    public void testHashCodeConsistent() {
        BigNumber a = new BigNumber(42);
        int hash1 = a.hashCode();
        int hash2 = a.hashCode();
        assertEquals(hash1, hash2);
    }

    @Test
    public void testHashCodeEqualObjects() {
        BigNumber a = new BigNumber(42);
        BigNumber b = new BigNumber(42);
        assertEquals(a.hashCode(), b.hashCode());
    }

    // ========== Negate Tests ==========

    @Test
    public void testNegatePositive() {
        BigNumber a = new BigNumber(42);
        BigNumber result = a.negate();
        assertEquals("-42", result.toString());
    }

    @Test
    public void testNegateNegative() {
        BigNumber a = new BigNumber(-42);
        BigNumber result = a.negate();
        assertEquals("42", result.toString());
    }

    @Test
    public void testNegateZero() {
        BigNumber result = BigNumber.zero.negate();
        assertEquals("-0", result.toString());
    }

    // ========== Property Tests ==========

    @Test
    public void testIsWhole() {
        assertTrue(new BigNumber(42).isWhole());
        assertFalse(BigNumber.parse("3.14").isWhole());
    }

    @Test
    public void testIsExactClaude() {
        assertTrue(new BigNumber(42).isExact());
        assertTrue(BigNumber.parse("3.14").isExact());
        // Division that doesn't terminate should not be exact
        BigNumber result = BigNumber.one.divide(new BigNumber(3));
        assertFalse(result.isExact());
    }


    // ========== Conversion Tests ==========

    @Test
    public void testIntValueClaude() {
        BigNumber bn = new BigNumber(42);
        assertEquals(42, bn.intValue());
    }

    @Test
    public void testLongValueClaude() {
        BigNumber bn = new BigNumber(123456789);
        assertEquals(123456789L, bn.longValue());
    }

    @Test
    public void testFloatValueClaude() {
        BigNumber bn = BigNumber.parse("3.14");
        assertEquals(3.14f, bn.floatValue(), 0.01f);
    }

    @Test
    public void testDoubleValueClaude() {
        BigNumber bn = BigNumber.parse("3.14159");
        assertEquals(3.14159, bn.doubleValue(), 0.00001);
    }

    // ========== toString Tests ==========

    @Test
    public void testToStringWholeNumber() {
        BigNumber bn = new BigNumber(42);
        assertEquals("42", bn.toString());
    }

    @Test
    public void testToStringNegativeNumber() {
        BigNumber bn = new BigNumber(-42);
        assertEquals("-42", bn.toString());
    }

    @Test
    public void testToStringDecimalNumber() {
        BigNumber bn = BigNumber.parse("3.14159");
        assertEquals("3.14159", bn.toString());
    }

    @Test
    public void testToStringZero() {
        assertEquals("0", BigNumber.zero.toString());
    }

    // ========== Edge Cases ==========

    // Ignored
    public void testVeryLargeNumber() {
        String large = "999999999999999999999999999999999999999999999999999999999999";
        BigNumber bn = BigNumber.parse(large);
        assertEquals(large, bn.toString());
    }

    @Test
    public void testVerySmallDecimal() {
        String small = "0.000000000000000000000000000001";
        BigNumber bn = BigNumber.parse(small);
        assertEquals(small, bn.toString());
    }

    @Test
    public void testAdditionCommutative() {
        BigNumber a = BigNumber.parse("123.456");
        BigNumber b = BigNumber.parse("789.012");
        assertEquals(a.add(b), b.add(a));
    }

    @Test
    public void testMultiplicationCommutative() {
        BigNumber a = BigNumber.parse("12.34");
        BigNumber b = BigNumber.parse("56.78");
        assertEquals(a.multiply(b), b.multiply(a));
    }

    @Test
    public void testAdditionAssociative() {
        BigNumber a = new BigNumber(1);
        BigNumber b = new BigNumber(2);
        BigNumber c = new BigNumber(3);
        assertEquals(a.add(b).add(c), a.add(b.add(c)));
    }

    @Test
    public void testMultiplicationAssociative() {
        BigNumber a = new BigNumber(2);
        BigNumber b = new BigNumber(3);
        BigNumber c = new BigNumber(4);
        assertEquals(a.multiply(b).multiply(c), a.multiply(b.multiply(c)));
    }

    @Test
    public void testDistributiveLaw() {
        BigNumber a = new BigNumber(2);
        BigNumber b = new BigNumber(3);
        BigNumber c = new BigNumber(4);
        // a * (b + c) = a * b + a * c
        BigNumber left = a.multiply(b.add(c));
        BigNumber right = a.multiply(b).add(a.multiply(c));
        assertEquals(left, right);
    }
}
