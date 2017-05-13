import com.mogproject.mogami._
import com.mogproject.mogami.core.state.StateCache.Implicits._

import com.mogproject.mogami.mate.MateSolver

val s1 = State.parseCsaString(Seq(
  "P1 *  *  *  *  *  * -KY *  * ",
  "P2 *  *  *  *  *  * -KY-OU * ",
  "P3 *  *  *  *  *  *  * -KY+FU",
  "P4 *  *  *  *  * -FU *  * +TO",
  "P5 *  *  *  *  *  *  *  *  * ",
  "P6 *  *  *  *  *  *  *  *  * ",
  "P7 *  *  *  *  *  *  *  *  * ",
  "P8 *  *  *  *  *  *  *  *  * ",
  "P9 *  *  *  *  *  *  *  *  * ",
  "P+00KA00KI",
  "P-00HI00KA00GI00GI00GI00KE00KE00KE00KE" +
    "00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU" +
    "00FU00FU",
  "+"
).mkString("\n"))



val s2 = State.parseCsaString(
  """
    |P1 *  *  *  *  * +KA * -KE *.
    |P2 *  *  *  *  *  *  *  * -OU
    |P3 *  *  *  *  *  * +HI-FU-FU
    |P4 *  *  *  *  *  *  *  *  *.
    |P5 *  *  *  *  *  *  *  *  *.
    |P6 *  *  *  *  *  *  *  *  *.
    |P7 *  *  *  *  *  *  *  *  *.
    |P8 *  *  *  *  *  *  *  *  *.
    |P9 *  *  *  *  *  *  *  *  *.
    |P+
    |P-00HI00KA00KI00KI00KI00KI00GI00GI00GI00GI00KE00KE00KE00KY00KY00KY00KY00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU
    |+""".stripMargin
)

val s3 = State.parseSfenString("1+P2Ss2l/1+S5b1/k1p4p1/1p+r1G2n1/p7p/P1N2S3/KPP1PP+pBP/7+r1/LNg6 b GNL2Pgl3p")


val r = MateSolver.solve(s3)
r.map(_.toJapaneseNotationString)






