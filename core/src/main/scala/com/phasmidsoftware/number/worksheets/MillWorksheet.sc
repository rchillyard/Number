/**
  * This is an example worksheet for Mill.
  */

import com.phasmidsoftware.number.mill.CoreMill

// Should be Some(102)...
CoreMill.parse("12  34  +  56  +  78  -  90  +  12  -  ").toOption.flatMap(_.evaluate).map(_.materialize)

// Should be Some(102)...
CoreMill.parseInfix("12 + 34  +  56  -  78  +  90  -  12").toOption.flatMap(_.evaluate).map(_.materialize)
