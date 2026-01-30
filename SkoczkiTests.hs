module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

-- =========================
-- Typy danych (skopiowane)
-- =========================

data Color = Bialy | Czarny deriving (Eq, Show)
data Piece = PW | PB | Empty deriving (Eq, Show)
type Pos = (Int, Int)
type Board = [[Piece]]
data Move = Move [Pos] deriving (Eq, Show)

-- =========================
-- Funkcje pomocnicze
-- =========================

pieceOf :: Color -> Piece
pieceOf Bialy = PW
pieceOf Czarny = PB

other :: Color -> Color
other Bialy = Czarny
other Czarny = Bialy

emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Empty)

getAtUnsafe :: Pos -> Board -> Piece
getAtUnsafe (x,y) b = (b !! (y-1)) !! (x-1)

setAt :: Pos -> Piece -> Board -> Board
setAt (x,y) v b =
  let row  = b !! (y-1)
      row' = take (x-1) row ++ [v] ++ drop x row
  in take (y-1) b ++ [row'] ++ drop y b

inBounds :: Pos -> Bool
inBounds (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

getAt :: Pos -> Board -> Either String Piece
getAt p b =
  if inBounds p then Right (getAtUnsafe p b)
  else Left "poza plansza"

placeMany :: Board -> [(Pos, Piece)] -> Board
placeMany b xs = foldl (\acc (p,pc) -> setAt p pc acc) b xs

allPiecesOf :: Color -> Board -> [Pos]
allPiecesOf c b =
  [ (x,y)
  | y <- [1..8]
  , x <- [1..8]
  , getAtUnsafe (x,y) b == pieceOf c
  ]

targetRows :: Color -> [Int]
targetRows Bialy = [7,8]
targetRows Czarny = [1,2]

hasWon :: Color -> Board -> Bool
hasWon c b =
  let ps = allPiecesOf c b
  in not (null ps) && all (\(_,r) -> r `elem` targetRows c) ps

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn sep (c:cs)
  | c == sep  = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitOn sep cs

parseCol :: Char -> Either String Int
parseCol ch =
  case ch of
    'A' -> Right 1; 'B' -> Right 2; 'C' -> Right 3; 'D' -> Right 4
    'E' -> Right 5; 'F' -> Right 6; 'G' -> Right 7; 'H' -> Right 8
    'a' -> Right 1; 'b' -> Right 2; 'c' -> Right 3; 'd' -> Right 4
    'e' -> Right 5; 'f' -> Right 6; 'g' -> Right 7; 'h' -> Right 8
    _   -> Left "kolumna musi byc A..H"

parseRow :: Char -> Either String Int
parseRow ch =
  case ch of
    '1' -> Right 1; '2' -> Right 2; '3' -> Right 3; '4' -> Right 4
    '5' -> Right 5; '6' -> Right 6; '7' -> Right 7; '8' -> Right 8
    _   -> Left "wiersz musi byc 1..8"

parsePos :: String -> Either String Pos
parsePos t =
  case t of
    (c:r:[]) -> do
      col <- parseCol c
      row <- parseRow r
      return (col,row)
    (c:r:rest) ->
      if rest == ""
        then do col <- parseCol c; row <- parseRow r; return (col,row)
        else Left "pozycja ma format np. E2"
    _ -> Left "pozycja ma format np. E2"

parseMove :: String -> Either String Move
parseMove s =
  let parts = splitOn '-' (trim s)
  in if length parts < 2
       then Left "ruch musi miec min. 2 pola, np. E2-E3"
       else do
         ps <- mapM parsePos parts
         return (Move ps)

showCol :: Int -> Char
showCol n =
  case n of
    1 -> 'A'; 2 -> 'B'; 3 -> 'C'; 4 -> 'D'
    5 -> 'E'; 6 -> 'F'; 7 -> 'G'; 8 -> 'H'
    _ -> '?'

showRow :: Int -> Char
showRow n =
  case n of
    1 -> '1'; 2 -> '2'; 3 -> '3'; 4 -> '4'
    5 -> '5'; 6 -> '6'; 7 -> '7'; 8 -> '8'
    _ -> '?'

showPos :: Pos -> String
showPos (c,r) = [showCol c, showRow r]

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs

showMove :: Move -> String
showMove (Move ps) = joinWith "-" (map showPos ps)

isJumpSegment :: (Pos, Pos) -> Bool
isJumpSegment (a,b) =
  let dc = abs (fst b - fst a)
      dr = abs (snd b - snd a)
  in (dc == 2 && dr == 0) || (dc == 0 && dr == 2)

validateSegments :: [Pos] -> Board -> Either String ()
validateSegments [_] _ = Right ()
validateSegments (a:b:rest) brd = do
  dest <- getAt b brd
  if dest /= Empty then Left "wejscie na zajete pole" else Right ()

  let dc = fst b - fst a
      dr = snd b - snd a
      adc = abs dc
      adr = abs dr

  let isStep = (adc == 1 && adr == 0) || (adc == 0 && adr == 1)
  let isJump = (adc == 2 && adr == 0) || (adc == 0 && adr == 2)

  if not (isStep || isJump)
    then Left "dozwolone sa tylko ruchy poziome/pionowe o 1 lub skoki o 2"
    else Right ()

  if isJump
    then do
      let mid = (fst a + dc `div` 2, snd a + dr `div` 2)
      mp <- getAt mid brd
      if mp == Empty
        then Left "skok wymaga pionka na polu posrednim"
        else validateSegments (b:rest) brd
    else
      validateSegments (b:rest) brd

applyMove :: Color -> Move -> Board -> Either String Board
applyMove c (Move ps) b = do
  if length ps < 2 then Left "ruch za krotki" else Right ()
  if any (not . inBounds) ps then Left "pozycja poza plansza" else Right ()

  let start = head ps
      end   = last ps 

  sp <- getAt start b
  if sp /= pieceOf c
    then Left "na polu startowym nie ma pionka gracza w ruchu"
    else Right ()

  ep <- getAt end b
  if ep /= Empty
    then Left "pole docelowe nie jest puste"
    else Right ()

  if length ps > 2 && any (not . isJumpSegment) (zip ps (tail ps))
    then Left "ruch wieloodcinkowy musi skladac sie wylacznie ze skokow"
    else Right ()

  validateSegments ps b

  let b1 = setAt start Empty b
  return (setAt end (pieceOf c) b1)

-- =========================
-- System testowy
-- =========================

data TestResult = Pass String | Fail String String
  deriving (Eq, Show)

assertEqual :: (Eq a, Show a) => String -> a -> a -> TestResult
assertEqual name expected actual =
  if expected == actual
    then Pass name
    else Fail name ("expected " ++ show expected ++ " but got " ++ show actual)

runTest :: String -> Bool -> TestResult
runTest name True = Pass name
runTest name False = Fail name "assertion failed"

-- =========================
-- TESTY
-- =========================

testParsePos :: [TestResult]
testParsePos =
  [ assertEqual "parsePos A1" (Right (1,1)) (parsePos "A1")
  , assertEqual "parsePos E4" (Right (5,4)) (parsePos "E4")
  , assertEqual "parsePos H8" (Right (8,8)) (parsePos "H8")
  , assertEqual "parsePos lowercase e2" (Right (5,2)) (parsePos "e2")
  , runTest "parsePos invalid I9" (case parsePos "I9" of Left _ -> True; Right _ -> False)
  , runTest "parsePos empty" (case parsePos "" of Left _ -> True; Right _ -> False)
  ]

testParseMove :: [TestResult]
testParseMove =
  [ assertEqual "parseMove E2-E3" (Right (Move [(5,2), (5,3)])) (parseMove "E2-E3")
  , assertEqual "parseMove E2-E4-G4" (Right (Move [(5,2), (5,4), (7,4)])) (parseMove "E2-E4-G4")
  , runTest "parseMove single pos" (case parseMove "E2" of Left _ -> True; Right _ -> False)
  , assertEqual "parseMove with spaces" (Right (Move [(5,2), (5,3)])) (parseMove " E2-E3 ")
  ]

testShowFunctions :: [TestResult]
testShowFunctions =
  [ assertEqual "showPos (5,4)" "E4" (showPos (5,4))
  , assertEqual "showPos (1,1)" "A1" (showPos (1,1))
  , assertEqual "showMove simple" "E2-E3" (showMove (Move [(5,2), (5,3)]))
  , assertEqual "showMove multi-jump" "E2-E4-G4" (showMove (Move [(5,2), (5,4), (7,4)]))
  ]

testBoardOperations :: [TestResult]
testBoardOperations =
  [ assertEqual "getAt in bounds" (Right Empty) (getAt (5,5) emptyBoard)
  , runTest "getAt out of bounds" (case getAt (9,9) emptyBoard of Left _ -> True; Right _ -> False)
  , let b = setAt (5,5) PW emptyBoard
    in assertEqual "setAt and getAt" PW (getAtUnsafe (5,5) b)
  ]

testAllPiecesOf :: [TestResult]
testAllPiecesOf =
  [ assertEqual "empty board" [] (allPiecesOf Bialy emptyBoard)
  , let b = setAt (5,5) PW emptyBoard
    in assertEqual "one piece" [(5,5)] (allPiecesOf Bialy b)
  , let b = placeMany emptyBoard [((1,1), PW), ((2,2), PW), ((3,3), PB)]
    in assertEqual "multiple pieces count" 2 (length (allPiecesOf Bialy b))
  ]

testHasWon :: [TestResult]
testHasWon =
  [ assertEqual "empty board no winner" False (hasWon Bialy emptyBoard)
  , let b = placeMany emptyBoard [((1,7), PW), ((2,8), PW)]
    in assertEqual "all white pieces in winning zone" True (hasWon Bialy b)
  , let b = placeMany emptyBoard [((1,7), PW), ((2,5), PW)]
    in assertEqual "white pieces not all in winning zone" False (hasWon Bialy b)
  , let b = placeMany emptyBoard [((3,1), PB), ((4,2), PB)]
    in assertEqual "all black pieces in winning zone" True (hasWon Czarny b)
  , let b = placeMany emptyBoard [((3,1), PB), ((4,5), PB)]
    in assertEqual "black pieces not all in winning zone" False (hasWon Czarny b)
  ]

testApplyMoveStep :: [TestResult]
testApplyMoveStep =
  [ let b = setAt (5,4) PW emptyBoard
        m = Move [(5,4), (5,5)]
        result = case applyMove Bialy m b of
                   Right b' -> getAtUnsafe (5,5) b'
                   Left _ -> Empty
    in assertEqual "valid step up" PW result
  , let b = setAt (5,4) PW emptyBoard
        m = Move [(5,4), (6,4)]
        result = case applyMove Bialy m b of
                   Right b' -> getAtUnsafe (6,4) b'
                   Left _ -> Empty
    in assertEqual "valid step right" PW result
  , runTest "invalid diagonal" (case applyMove Bialy (Move [(5,4), (6,5)]) (setAt (5,4) PW emptyBoard) of
                                  Left _ -> True
                                  Right _ -> False)
  , runTest "no piece at start" (case applyMove Bialy (Move [(5,4), (5,5)]) emptyBoard of
                                   Left _ -> True
                                   Right _ -> False)
  , runTest "destination occupied" (case applyMove Bialy (Move [(5,4), (5,5)]) (placeMany emptyBoard [((5,4), PW), ((5,5), PB)]) of
                                      Left _ -> True
                                      Right _ -> False)
  ]

testApplyMoveJump :: [TestResult]
testApplyMoveJump =
  [ let b = placeMany emptyBoard [((5,4), PW), ((5,5), PB)]
        m = Move [(5,4), (5,6)]
        result = case applyMove Bialy m b of
                   Right b' -> getAtUnsafe (5,6) b'
                   Left _ -> Empty
    in assertEqual "valid jump over piece" PW result
  , runTest "invalid jump over empty" (case applyMove Bialy (Move [(5,4), (5,6)]) (setAt (5,4) PW emptyBoard) of
                                         Left _ -> True
                                         Right _ -> False)
  , let b = placeMany emptyBoard [((5,4), PW), ((4,4), PB)]
        m = Move [(5,4), (3,4)]
        result = case applyMove Bialy m b of
                   Right b' -> getAtUnsafe (3,4) b'
                   Left _ -> Empty
    in assertEqual "jump left" PW result
  ]

testApplyMoveMultiJump :: [TestResult]
testApplyMoveMultiJump =
  [ let b = placeMany emptyBoard [((5,4), PW), ((5,5), PB), ((6,6), PW)]
        m = Move [(5,4), (5,6), (7,6)]
        result = case applyMove Bialy m b of
                   Right b' -> getAtUnsafe (7,6) b'
                   Left _ -> Empty
    in assertEqual "valid multi-jump" PW result
  , runTest "multi-segment with step should fail" (case applyMove Bialy (Move [(5,4), (5,6), (5,7)]) (placeMany emptyBoard [((5,4), PW), ((5,5), PB)]) of
                                                      Left _ -> True
                                                      Right _ -> False)
  ]

testInBounds :: [TestResult]
testInBounds =
  [ assertEqual "inBounds (1,1)" True (inBounds (1,1))
  , assertEqual "inBounds (8,8)" True (inBounds (8,8))
  , assertEqual "inBounds (0,5)" False (inBounds (0,5))
  , assertEqual "inBounds (5,9)" False (inBounds (5,9))
  , assertEqual "inBounds (-1,5)" False (inBounds (-1,5))
  ]

-- =========================
-- Test runner
-- =========================

printResult :: TestResult -> IO ()
printResult (Pass name) = putStrLn ("    ✓ " ++ name)
printResult (Fail name reason) = putStrLn ("    ✗ " ++ name ++ " (" ++ reason ++ ")")

runTestGroup :: String -> [TestResult] -> IO (Int, Int)
runTestGroup name results = do
  putStrLn ("  [RUN] " ++ name)
  mapM_ printResult results
  let passes = length [r | r@(Pass _) <- results]
      fails = length [r | r@(Fail _ _) <- results]
  return (passes, fails)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "=== Testy Skoczki ==="
  putStrLn ""
  
  (p1, f1) <- runTestGroup "parsePos" testParsePos
  (p2, f2) <- runTestGroup "parseMove" testParseMove
  (p3, f3) <- runTestGroup "showFunctions" testShowFunctions
  (p4, f4) <- runTestGroup "boardOperations" testBoardOperations
  (p5, f5) <- runTestGroup "allPiecesOf" testAllPiecesOf
  (p6, f6) <- runTestGroup "hasWon" testHasWon
  (p7, f7) <- runTestGroup "applyMoveStep" testApplyMoveStep
  (p8, f8) <- runTestGroup "applyMoveJump" testApplyMoveJump
  (p9, f9) <- runTestGroup "applyMoveMultiJump" testApplyMoveMultiJump
  (p10, f10) <- runTestGroup "inBounds" testInBounds
  
  let totalPass = p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10
      totalFail = f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + f10
  
  putStrLn ""
  putStrLn ("Total: " ++ show totalPass ++ " passed, " ++ show totalFail ++ " failed")
  putStrLn ""
  
  if totalFail == 0
    then putStrLn "✓ Wszystkie testy zaliczone!"
    else putStrLn "✗ Niektore testy nie przeszly."
