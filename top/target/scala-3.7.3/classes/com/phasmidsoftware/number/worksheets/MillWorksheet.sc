/**
  * This is an example worksheet for Mill.
  */

import com.phasmidsoftware.number.core.mill.{CoreMill, CoreMillExpression}

// Should be Some(102)...
val maybeMill1: Option[CoreMill] = CoreMill.parse("12  34  +  56  +  78  -  90  +  12  -  ").toOption
val maybeExpression1: Option[CoreMillExpression] = maybeMill1.flatMap(_.evaluate)
val x1 = maybeExpression1.map(_.value)

// Should be Some(102)...
val maybeMill2: Option[CoreMill] = CoreMill.parseInfix("12 + 34  +  56  -  78  +  90  -  12").toOption
val maybeExpression2: Option[CoreMillExpression] = maybeMill2.flatMap(_.evaluate)
val x2 = maybeExpression2.map(_.value)
