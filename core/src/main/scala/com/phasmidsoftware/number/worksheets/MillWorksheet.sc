/**
  * This is an example worksheet for Mill.
  */

import com.phasmidsoftware.number.mill.Mill

// Should be Some(102)...
Mill.parse("12  34  +  56  +  78  -  90  +  12  -  ").toOption.flatMap(_.evaluate).map(_.materialize)

// Should be Some(102)...
Mill.parseInfix("12 + 34  +  56  -  78  +  90  -  12").toOption.flatMap(_.evaluate).map(_.materialize)
