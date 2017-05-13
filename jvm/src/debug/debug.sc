import com.mogproject.mogami._
import com.mogproject.mogami.core.state.StateCache.Implicits._

import com.mogproject.mogami.mate.MateSolver


val s1 = State.parseSfenString("4k4/9/9/9/3+PP4/9/9/9/9 b 4G2r2b4s4n4l16p") // mate in 9
val s2 = State.parseSfenString("8k/7p1/1r7/5bS2/7N1/9/9/9/9 b RSNLb4g2s2n3l17p") // mate in 7

val r1 = MateSolver.solve(s1)
r1.get.map(_.toJapaneseNotationString)

val r2 = MateSolver.solve(s2)
r2.get.map(_.toJapaneseNotationString)


