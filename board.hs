import Data.Foldable
import Data.Sequence
import Data.Char (isSpace)
import Control.Monad
import Display
import Rook
import Bishop
import Knight
import King
import Queen
import Pawn

type Board = [[Maybe Piece]]
data Piece = Piece{color::Color,player::Player}

data Color = White | Black
data Player = King | Queen | Rook | Knight | Bishop | Pawn

instance Show Color where
	show White = "White"
	show Black = "Black"

instance Eq Color where
	White == White = True
	Black == Black = True
	_ == _ = False

instance Show Player where
	show King = "King"
	show Queen = "Queen"
	show Rook = "Rook"
	show Knight = "Knight"
	show Bishop = "Bishop"
	show Pawn = "Pawn"

instance Eq Player where
	King == King = True
	Queen == Queen = True
	Rook == Rook = True
	Knight == Knight = True
	Bishop == Bishop = True
	Pawn == Pawn = True
	_ == _ = False

instance Show Piece where
	show Piece{color=White,player=King}= "\9812"
	show Piece{color=White,player=Queen} = "\9813"
	show Piece{color=White,player=Bishop} = "\9815"
	show Piece{color=White,player=Rook} = "\9814"
	show Piece{color=White,player=Knight} = "\9816"
	show Piece{color=White,player=Pawn} = "\9817"
	show Piece{color=Black,player=King} = "\9818"
	show Piece{color=Black,player=Queen} = "\9819"
	show Piece{color=Black,player=Bishop} = "\9821"
	show Piece{color=Black,player=Rook} = "\9820"
	show Piece{color=Black,player=Knight} = "\9822"
	show Piece{color=Black,player=Pawn} = "\9823"


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
initialBoard = [[Just br,Just bn,Just bb,Just bq,Just bk,Just bb,Just bn, Just br],
				[Just bp,Just bp,Just bp,Just bp,Just bp,Just bp,Just bp, Just bp],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Just wp,Just wp,Just wp,Just wp,Just wp,Just wp,Just wp, Just wp],
				[Just wr,Just wn,Just wb,Just wq,Just wk,Just wb,Just wn, Just wr]]


changeBoard::Int -> Int -> Int -> Int -> Board -> Board
changeBoard x1 y1 x2 y2 a = do
		let p = (a!!x1)!!y1
		let b2 | x1 == x2  = toList $ update y1 Nothing $ fromList (toList $ update y2 p $ fromList (a!!x2))
			   | otherwise = toList $ update y2 p $ fromList (a!!x2)
		let b1 | x1 == x2  = toList $ update y1 Nothing $ fromList (toList $ update y2 p $ fromList (a!!x2))
			   | otherwise = toList $ update y1 Nothing $ fromList (a!!x1)
		let f = \x -> case () of () | x == x1 -> b1 | x == x2 -> b2 |otherwise -> a!!x
		let b = ([f 0] ++ [f 1] ++ [f 2] ++ [f 3] ++ [f 4] ++ [f 5] ++ [f 6] ++ [f 7])
		b

pawnPromotion:: Int -> Int -> Board -> Color -> String -> Board
pawnPromotion x2 y2 a col line = do
		let trim = f . f
   			where f = Prelude.reverse . dropWhile isSpace
   		let promoted_piece x = case x of 
   						"Rook" -> (if col == White then (Just wr) else (Just br))
   						"Bishop" -> (if col == White then (Just wb) else (Just bb))
   						"Knight" -> (if col == White then (Just wn) else (Just bn))
   						"Queen" -> (if col == White then (Just wq) else (Just bq))
   			      			--otherwise -> do putStrLn "Wrong choice - choose again"
   			      			--     		       pawnPromotion x2 y2 a col

		let b1 = toList $ update y2 (promoted_piece $ trim line) $ fromList (a!!x2)
		let f = \x -> case () of () | x == x2 -> b1 |otherwise -> a!!x
		let b = ([f 0] ++ [f 1] ++ [f 2] ++ [f 3] ++ [f 4] ++ [f 5] ++ [f 6] ++ [f 7])
		b

nextMove:: Board -> Bool -> IO()
nextMove b chance = do
		c <- getLine
		let x = (words $ c)
		move x b (chance)


convert::Maybe Piece -> String
convert (Just x) = show x
convert Nothing = " "

position1::Char -> Int
position1 x = do
	let y | x == '8' = 0   
		  | x == '7' = 1
		  | x == '6' = 2
		  | x == '5' = 3
		  | x == '4' = 4
		  | x == '3' = 5
		  | x == '2' = 6
		  | x == '1' = 7
	y

position2::Char -> Int
position2 x = do
	let y | x == 'a' = 0
		  | x == 'b' = 1
		  | x == 'c' = 2
		  | x == 'd' = 3
		  | x == 'e' = 4
		  | x == 'f' = 5
		  | x == 'g' = 6
		  | x == 'h' = 7
	y



move::[String] -> Board -> Bool -> IO()
move x b chance = do
		let x1 = (x!!0)
	 	let x2 = (x!!1)
	 	let a1 = position1 (x1!!1)
	 	let a2 = position2 (x1!!0)
	 	let a3 = position1 (x2!!1)
	 	let a4 = position2 (x2!!0)

	 	let inRange = a1 `Prelude.elem` [0..7] && a2 `Prelude.elem` [0..7] && a3 `Prelude.elem` [0..7] && a4 `Prelude.elem` [0..7]

	 	if inRange && (a1,a2) /= (a3,a4)
	 		then do
	 			let initial_empty = (convert ((b!!a1)!!a2)) == " "
	 			if (not initial_empty)
	 				then do
	 					let col1 = color $ (\(Just x) -> x) ((b!!a1)!!a2)

	 					let final_empty = (convert ((b!!a3)!!a4)) == " "

	 					let col2 | final_empty = if col1 == White then Black else White
	 						 	 | otherwise   = color $ (\(Just x) -> x) ((b!!a3)!!a4)

	 					let play1 = player $ (\(Just x) -> x) ((b!!a1)!!a2)

	 					let valid_move | play1 == Rook = Rook.validPath a1 a2 a3 a4 b
	 							| play1 == Bishop = Bishop.validPath a1 a2 a3 a4 b
								| play1 == Knight = Knight.validPath a1 a2 a3 a4 b
								| play1 == King = King.validPath a1 a2 a3 a4 b
								| play1 == Queen = Queen.validPath a1 a2 a3 a4 b
								| play1 == Pawn = Pawn.validPath a1 a2 a3 a4 b (if col1 == White then 1 else (-1))
	 							| otherwise = True

					 	if ((chance == True && col1 == White) || (chance == False && col1 == Black)) && col1 /= col2 && valid_move
					 		then do
					 			let board = changeBoard a1 a2 a3 a4 b
					 			let board' = (map.map) convert board

					 			putStrLn "   a  b  c  d  e  f  g  h"
					 			let print' n  t  r | n == 7     =  Display.prints ((board'!!n)) t r
		         							        | otherwise = do
											Display.prints ((board'!!n)) t r
											print' (n+1) (not t) (r-1)

					 			print' 0 True 8
					 			putStrLn "   a  b  c  d  e  f  g  h"

					 			if (not final_empty && (player $ (\(Just x)->x) ((b!!a3)!!a4)) == King) 
					 				then do
					 					putStrLn ((show col1) ++ " Wins !!!!")
					 				else do
							 			if (play1 == Pawn && col1 == White && a3 == 7) || (play1 == Pawn && col1 == Black && a3 == 0)
							 				then do
							 					putStrLn "What do you want - Rook, Bishop, Knight, Queen?"
							 					choice <- getLine

							 					let board'' = pawnPromotion a3 a4 board col1 choice
							 					let board' = (map.map) convert board''

							 					putStrLn "   a  b  c  d  e  f  g  h"
							 					let print' n  t  r | n == 7     =  Display.prints ((board'!!n)) t r
				         							        		         | otherwise = do
																Display.prints ((board'!!n)) t r
																print' (n+1) (not t) (r-1)

							 					print' 0 True 8
							 					putStrLn "   a  b  c  d  e  f  g  h"

							 					when ((not chance) == True) $ putStrLn "White to play" 
							 					when ((not chance) == False) $ putStrLn "Black to play"
							 					nextMove board'' (not chance)
							 				else do
									 			when ((not chance) == True) $ putStrLn "White to play" 
							 					when ((not chance) == False) $ putStrLn "Black to play"
									 			nextMove board (not chance)
					 		else do 
					 			putStrLn "Invalid Input - Play again" 
					 			nextMove b (chance)
	 				else do 
	 					putStrLn "Don't move empty spaces - Play again"
	 					nextMove b (chance)
	 		else do 
	 			putStrLn "Not possible - Play again"
	 			nextMove b (chance)


main::IO()
main = do
	 putStrLn "   a  b  c  d  e  f  g  h"
	 let board' = (map.map) convert initialBoard
	 let print' n  t  r| n == 7     =  prints ((board'!!n)) t r
		         | otherwise = do
					prints ((board'!!n)) t r
					print' (n+1) (not t) (r-1)
	 print' 0 True 8
	 putStrLn "   a  b  c  d  e  f  g  h"

	 putStrLn "White to play"
	 c <- getLine
	 let x = (words $ c)
	 let x1 = (x!!0)
	 let x2 = (x!!1)

	 -- True -> White to play, False -> Black to play
	 let chance = True
	 let a1 = position1 (x1!!1)
	 let a2 = position2 (x1!!0)
	 let a3 = position1 (x2!!1)
	 let a4 = position2 (x2!!0)

	 let inRange = a1 `Prelude.elem` [0..7] && a2 `Prelude.elem` [0..7] && a3 `Prelude.elem` [0..7] && a4 `Prelude.elem` [0..7]

	 if inRange && (a1,a2) /= (a3,a4)
	 	then do
	 		let initial_empty = (convert ((initialBoard!!a1)!!a2)) == " "
	 		if (not initial_empty)
	 			then do
	 				let col1 = color $ (\(Just x) -> x) ((initialBoard!!a1)!!a2)

	 				let final_empty = (convert ((initialBoard!!a3)!!a4)) == " "

	 				let col2 | final_empty = Black
	 						 | otherwise   = color $ (\(Just x) -> x) ((initialBoard!!a3)!!a4)

	 				let play1 = player $ (\(Just x) -> x) ((initialBoard!!a1)!!a2)

	 				let valid_move | play1 == Rook = Rook.validPath a1 a2 a3 a4 initialBoard
	 							   | play1 == Bishop = Bishop.validPath a1 a2 a3 a4 initialBoard
								   | play1 == Knight = Knight.validPath a1 a2 a3 a4 initialBoard
								   | play1 == King = King.validPath a1 a2 a3 a4 initialBoard
								   | play1 == Queen = Queen.validPath a1 a2 a3 a4 initialBoard
								   | play1 == Pawn = Pawn.validPath a1 a2 a3 a4 initialBoard (if col1 == White then 1 else (-1))
	 							   | otherwise = True

			 		if( chance == True && col1 == White && col2 == Black && valid_move)
			 			then do
			 				let board = changeBoard a1 a2 a3 a4 initialBoard
					 		let board' = (map.map) convert board

					 		putStrLn "   a  b  c  d  e  f  g  h"
					 		let print' n t r | n == 7     =  Display.prints ((board'!!n)) t r
					 			        | otherwise = do
					 			  		Display.prints ((board'!!n)) t r
					 			  		print' (n+1) (not t) (r-1)

					 		print' 0 True 8
					 		putStrLn "   a  b  c  d  e  f  g  h"

					 		when ((not chance) == True) $ putStrLn "White to play" 
							when ((not chance) == False) $ putStrLn "Black to play"
					 		nextMove board (not chance)
					 	else do 
					 		putStrLn "Invalid Input - Play again"
					 		nextMove initialBoard (chance)
	 			else do 
	 				putStrLn "Don't move empty spaces - Play again"
	 				nextMove initialBoard (chance)
		else do 
			putStrLn "Not possible - Play again"
			nextMove initialBoard (chance)

