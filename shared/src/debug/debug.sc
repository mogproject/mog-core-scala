import com.mogproject.mogami.core._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.Square.HAND

val g = Game(
  State(BLACK,
    Map(Square(10) -> Piece(WHITE,PAWN), Square(47) -> Piece(WHITE,PAWN), Square(42) -> Piece(WHITE,PAWN), Square(46) -> Piece(WHITE,PPAWN), Square(60) -> Piece(WHITE,PPAWN), Square(24) -> Piece(WHITE,PPAWN), Square(6) -> Piece(BLACK,PSILVER), Square(20) -> Piece(BLACK,PPAWN), Square(7) -> Piece(WHITE,KING), Square(77) -> Piece(BLACK,KNIGHT), Square(13) -> Piece(BLACK,PKNIGHT), Square(52) -> Piece(WHITE,LANCE), Square(45) -> Piece(BLACK,BISHOP), Square(11) -> Piece(BLACK,GOLD), Square(67) -> Piece(WHITE,PSILVER), Square(69) -> Piece(BLACK,LANCE), Square(12) -> Piece(WHITE,GOLD), Square(58) -> Piece(BLACK,SILVER), Square(5) -> Piece(BLACK,PLANCE), Square(51) -> Piece(BLACK,PAWN), Square(48) -> Piece(BLACK,KING), Square(15) -> Piece(WHITE,ROOK), Square(8) -> Piece(WHITE,GOLD), Square(40) -> Piece(WHITE,ROOK), Square(57) -> Piece(BLACK,PAWN), Square(37) -> Piece(BLACK,LANCE), Square(41) -> Piece(BLACK,PAWN), Square(36) -> Piece(BLACK,PKNIGHT), Square(28) -> Piece(BLACK,BISHOP), Square(16) -> Piece(BLACK,PKNIGHT), Square(80) -> Piece(BLACK,PAWN), Square(26) -> Piece(BLACK,PPAWN), Square(49) -> Piece(BLACK,PAWN), Square(34) -> Piece(BLACK,SILVER), Square(29) -> Piece(WHITE,PPAWN), Square(25) -> Piece(WHITE,PAWN), Square(43) -> Piece(BLACK,PAWN), Square(22) -> Piece(WHITE,GOLD)),
    Map(Piece(WHITE,ROOK) -> 0, Piece(WHITE,SILVER) -> 0, Piece(BLACK,LANCE) -> 0, Piece(BLACK,ROOK) -> 0, Piece(WHITE,GOLD) -> 0, Piece(WHITE,BISHOP) -> 0, Piece(WHITE,LANCE) -> 0, Piece(WHITE,PAWN) -> 1, Piece(BLACK,PAWN) -> 1, Piece(WHITE,KNIGHT) -> 0, Piece(BLACK,BISHOP) -> 0, Piece(BLACK,GOLD) -> 0, Piece(BLACK,SILVER) -> 0, Piece(BLACK,KNIGHT) -> 0)),
  Stream(
    ExtendedMove(BLACK,Square(16),Square(7),PKNIGHT,false,Some(KING),false,None),
    ExtendedMove(WHITE,Square(-1),Square(9),PAWN,false,None,false,None),
    ExtendedMove(BLACK,Square(34),Square(44),SILVER,false,None,false,None),
    ExtendedMove(WHITE,Square(12),Square(20),GOLD,false,Some(PPAWN),false,Some(9246)),
    ExtendedMove(BLACK,Square(-1),Square(32),KING,false,None,false,Some(7791)),
    ExtendedMove(WHITE,Square(15),Square(13),ROOK,false,Some(PKNIGHT),false,None),
    ExtendedMove(BLACK,Square(32),Square(40),KING,false,Some(ROOK),false,Some(2860)),
    ExtendedMove(WHITE,Square(-1),Square(14),KNIGHT,false,None,false,None),
    ExtendedMove(BLACK,Square(28),Square(20),BISHOP,false,Some(GOLD),false,None),
    ExtendedMove(WHITE,Square(14),Square(33),KNIGHT,false,None,false,None),
    ExtendedMove(BLACK,Square(-1),Square(16),GOLD,false,None,false,Some(834)),
    ExtendedMove(WHITE,Square(33),Square(50),KNIGHT,false,None,false,None),
    ExtendedMove(BLACK,Square(37),Square(10),LANCE,false,Some(PAWN),false,None),
    ExtendedMove(WHITE,Square(-1),Square(37),PAWN,false,None,false,Some(6948)),
    ExtendedMove(BLACK,Square(58),Square(68),SILVER,false,None,false,None),
    ExtendedMove(WHITE,Square(50),Square(69),PKNIGHT,true,Some(LANCE),false,Some(6624)), ExtendedMove(BLACK,Square(-1),Square(32),ROOK,false,None,false,None), ExtendedMove(WHITE,Square(-1),Square(31),LANCE,false,None,true,None), ExtendedMove(BLACK,Square(40),Square(50),KING,false,None,false,None), ExtendedMove(WHITE,Square(24),Square(23),PPAWN,false,None,false,None), ExtendedMove(BLACK,Square(68),Square(76),SILVER,false,None,false,None), ExtendedMove(WHITE,Square(67),Square(76),PSILVER,false,Some(SILVER),false,None), ExtendedMove(BLACK,Square(49),Square(40),PAWN,false,None,false,Some(9861)), ExtendedMove(WHITE,Square(8),Square(16),GOLD,false,Some(GOLD),false,Some(1274)), ExtendedMove(BLACK,Square(11),Square(12),GOLD,false,None,false,Some(4101)), ExtendedMove(WHITE,Square(47),Square(56),PAWN,false,None,false,None), ExtendedMove(BLACK,Square(48),Square(58),KING,false,None,false,None), ExtendedMove(WHITE,Square(-1),Square(54),SILVER,false,None,false,None), ExtendedMove(BLACK,Square(12),Square(13),GOLD,false,Some(ROOK),false,None), ExtendedMove(WHITE,Square(-1),Square(21),GOLD,false,None,false,Some(2203)), ExtendedMove(BLACK,Square(-1),Square(19),PAWN,false,None,false,None), ExtendedMove(WHITE,Square(31),Square(40),LANCE,false,Some(PAWN),false,Some(5749))),
  GameInfo(Map(
    'formatVersion -> "2.2",
    'event -> "lfqmpnstxboxuerooszFeJtoryvgnlcwmhjtLjavdltpakbTdu",
    'endTime -> "2003/05/03 11:11:05",
    'opening -> "YAGURA"
)),0)

val sf = g.toSfenString

val s = "gk+S+L5/1+Nr1+NgGp1/+Pp+p1g1+P2/1S4+pB1/1PpPr2L+N/1lP1PKp+pB/2+p1SP3/2L1+s4/P2N5 b Pp 0"

Game.parseSfenString(s)
//val t = s + " 8b8a P*1b 8d9e 4b3c K*6d 7b5b 6d5e N*6b 2d3c 6b7d G*8b 7d6f 2e2b P*2e 5g6h 6f7h+ R*6d L*5d 5e6f 7c6c 6h5i 5h5i 5f5e 9a8b 3b4b 3f3g 4f5g S*1g 4b5b G*4c P*2c 5d5e"
val t = s + " 8b8a P*1b 8d9e 4b3c"

val g2 = Game.parseSfenString(t).get

g2.toCsaString

g2.currentState.toCsaString


print(g2.currentState.hand)








