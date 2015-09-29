
type Board = [[Maybe Piece]]

data Piece = Piece{color::Color,player::Player} 
data Color = White | Black 
data Player = King | Queen | Rook | Knight | Bishop | Pawn 


instance Show Piece where
	show Piece{color=White,player=King}= "WK"
	show Piece{color=White,player=Queen} = "WQ"
	show Piece{color=White,player=Bishop} = "WB"
	show Piece{color=White,player=Rook} = "WR"
	show Piece{color=White,player=Knight} = "WN"
	show Piece{color=White,player=Pawn} = "WP"
	show Piece{color=Black,player=King} = "Bk"
	show Piece{color=Black,player=Queen} = "BQ"
	show Piece{color=Black,player=Bishop} = "BB"
	show Piece{color=Black,player=Rook} = "BR"
	show Piece{color=Black,player=Knight} = "BN"
	show Piece{color=Black,player=Pawn} = "BP"

wk = Piece{color=White,player=King}
wq = Piece{color=White,player=Queen}
wb = Piece{color=White,player=Bishop}
wr = Piece{color=White,player=Rook}
wn = Piece{color=White,player=Knight}
wp = Piece{color=White,player=Pawn}
bk = Piece{color=Black,player=King}
bq = Piece{color=Black,player=Queen}
bb = Piece{color=Black,player=Bishop}
br = Piece{color=Black,player=Rook}
bn = Piece{color=Black,player=Knight}
bp = Piece{color=Black,player=Pawn}

initialBoard :: Board
initialBoard = [[Just wr,Just wn,Just wb,Just wq,Just wk,Just wb,Just wn, Just wr],
				[Just wp,Just wp,Just wp,Just wp,Just wp,Just wp,Just wp, Just wp],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Just bp,Just bp,Just bp,Just bp,Just bp,Just bp,Just bp, Just bp],
				[Just br,Just bn,Just bb,Just bq,Just bk,Just bb,Just bn, Just br]]



