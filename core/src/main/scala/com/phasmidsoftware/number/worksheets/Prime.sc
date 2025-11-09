import com.phasmidsoftware.number.core.Primes
import scala.math.BigInt.int2bigInt

Primes.allPrimes.take(20).toList

val plist: LazyList[Int] = LazyList.from(BigInt(2).toInt) filter (_.isProbablePrime(30))

val p1000: List[Int] = plist.take(168).toList


val p524288: List[Int] = plist.takeWhile(_ < 524288).toList

p524288.last

val p524288Count: Int = p524288.length


//val pMillion: List[Int] = plist.take(1000000).toList

//val pLargerThan100000: LazyList[Int] = plist.dropWhile(p => p < 100000)

//pLargerThan100000.take(1).toList

p1000.foreach(println)

println(p524288Count)

