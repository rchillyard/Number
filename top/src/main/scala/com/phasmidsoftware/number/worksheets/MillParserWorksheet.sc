/**
  * This is an example worksheet for MillParser.
  */

import com.phasmidsoftware.number.core.parse.MillParser

val p = MillParser
p.parseMill("12  34  +  56  +  78  -  90  +  12  -  ")

p.parseMill("73 24 <> -")
