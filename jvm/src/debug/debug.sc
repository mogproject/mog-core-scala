import com.mogproject.mogami._

import com.mogproject.mogami.mate.MateSolver


val s1 = State.parseSfenString("4k4/9/9/9/3+PP4/9/9/9/9 b 4G2r2b4s4n4l16p") // mate in 9
val s2 = State.parseSfenString("8k/7p1/1r7/5bS2/7N1/9/9/9/9 b RSNLb4g2s2n3l17p") // mate in 7
val s3 = State.parseSfenString("1+P2Ss2l/1+S5b1/k1p4p1/1p+r1G2n1/p7p/P1N2S3/KPP1PP+pBP/7+r1/LNg6 b GNL2Pgl3p")
val s4 = State.parseSfenString("4RB1k1/5s3/7n1/5s1LP/9/7r1/9/9/6K2 b b4g2s3n3l17p") // Karolina, mate in 13
val s5 = State.parseSfenString("k1+P4n1/2L+P2sL1/r4+P+P1P/+BpP+Pl1+Rg1/NP1S+PP+p1g/2L+p1g+P1+P/Ps1G1+P1N1/1sN1P4/B8 b -") // Kemuri
val s6 = State.parseSfenString("g1+P1k1+P+P+L/1p3P3/+R+p2pp1pl/1NNsg+p2+R/+b+nL+P1+p3/1P3ssP1/2P1+Ps2N/4+P1P1L/+B5G1g b -") // Microcosmos, mate in 1535
val s7 = State.parseSfenString("4R1lk1/5s3/6+B2/7rP/9/9/9/9/6K2 b Sb4g2s4n3l17p")

val r7 = MateSolver.solve(s7)
r7.get.map(_.toJapaneseNotationString)

//val r2 = MateSolver.solve(s2)
//r2.get.map(_.toJapaneseNotationString)

//val r3 = MateSolver.solve(s3)
//r3.get.map(_.toJapaneseNotationString)



