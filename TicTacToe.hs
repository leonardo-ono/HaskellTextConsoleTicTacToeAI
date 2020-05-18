-- Haskell Tic Tac Toe AI (minimax)
--
-- references:
-- https://towardsdatascience.com/tic-tac-toe-creating-unbeatable-ai-with-minimax-algorithm-8af9e52c1e7d
--
-- TODO: maybe it could randomly select nodes with the same score later
--
-- Written by Leonardo Ono (ono.leo80@gmail.com)
-- May 18, 2020
-- GHC version 8.6.5

import Data.List
import Data.Bits
import Data.Char
import Text.Read
import Data.Maybe

--- Model ---

initialBoard :: (Int, Int)
initialBoard = (0, 0) 

checkWin p = or $ map (\w -> w .&. p == w) [7, 56, 73, 84, 146, 273, 292, 448]

availableMove (p0, p1) p = p0 .|. p1 /= p0 .|. p1 .|. p

availableMoves board' = filter (availableMove board') [2^x | x <- [0..8]] 

bestMove choice (minMax, minMax') (p0, p1) (s0, s1) moves = minMax $ check <$> moves
    where check move
            | checkWin (p0 + move) = (s0, choice')
            | null moves' = (0, choice')
            | otherwise = bestMove choice' (minMax', minMax) (p1, p0 + move) (s1, s0) moves'
                where moves' = delete move moves
                      choice' = if choice < 0 then move else choice

playAI board'@(p0, p1) = (p0 + aiMove, p1)
    where aiMove = snd $ bestMove  (-1) (maximum, minimum) board' (1, -1) (availableMoves board')

--- Text Console View ---

boardToString board'@(p0, p1) = "\nO=CPU X=Player\n\nBoard:     Available move(s):\n" ++
                                [s0,'|',s1,'|',s2] ++ "      " ++ 
                                [a0,'|',a1,'|',a2] ++ "\n-+-+-      -+-+-\n" ++
                                [s3,'|',s4,'|',s5] ++ "  ->  " ++ 
                                [a3,'|',a4,'|',a5] ++ "\n-+-+-      -+-+-\n" ++ 
                                [s6,'|',s7,'|',s8] ++ "      " ++ 
                                [a6,'|',a7,'|',a8]
    where [s0,s1,s2,s3,s4,s5,s6,s7,s8] = toSymbol <$> [2^x | x <- [0..8]] 
          toSymbol i | p0 .&. i == i = 'O' 
                     | p1 .&. i == i = 'X'
                     | otherwise = ' '
          [a0,a1,a2,a3,a4,a5,a6,a7,a8] = toAvailableMove <$> [0..8] 
          toAvailableMove i | availableMove board' (2^i) = chr (48 + i)
                            | otherwise = ' '                     

playGame :: (Int, Int) -> Int -> IO ()                             
playGame board'@(p0, p1) turn
    | checkWin p0 = putStrLn $ boardStr ++ "\n\nCPU WIN !\n"
    | checkWin p1 = putStrLn $ boardStr ++ "\n\nYOU WIN !\n"
    | null availableMoves' = putStrLn $ boardStr ++ "\n\nDRAW !\n"
    | otherwise = if turn == 0 then playAI' else playPlayer
        where boardStr = boardToString board'
              availableMoves' = availableMoves board'
              playAI' = playGame (playAI board') (xor 1 turn)
              playPlayer = do 
                  putStrLn $ boardStr ++ "\n\nyour move (0-8) ?"
                  moveStr <- getLine
                  let moveInt = (fromMaybe (-1) $ readMaybe moveStr) :: Int
                  if moveInt < 0 || not ((2^moveInt) `elem` availableMoves')
                  then do putStrLn "\nInvalid move !"
                          playGame board' turn
                  else do putStrLn "\nOk !"
                          playGame (p0, p1 + (2^moveInt)) (xor 1 turn)

main :: IO ()
main = playGame initialBoard 0
