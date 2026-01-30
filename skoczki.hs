module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

-- =========================
-- Typy danych (wsm najwazniejsze)
-- =========================

-- Color może być Bialy albo Czarny.
data Color = Bialy | Czarny
  deriving (Eq, Show)
  -- deriving Eq: mozemy uzywac == do porownan
  -- deriving Show: pozwala wypisać kolor jako String

-- Piece opisuje co stoi na polu planszy:
-- PW - biały pionek, PB - czarny pionek, Empty - puste.
data Piece = PW | PB | Empty
  deriving (Eq)

-- Pozycja to para (kolumna, wiersz), z zakresu liczb 1..8.
-- Kolumny A..H mapujemy na 1..8.
type Pos = (Int, Int)

-- Board: plansza to lista list pól.
type Board = [[Piece]]

-- Move to self explanatory jest
-- Move [(5,2),(5,3)]   == E2-E3
-- Move [(5,2),(5,4),(7,4)] == E2-E4-G4 - wieloskok
data Move = Move [Pos]
  deriving (Eq, Show)


main :: IO ()
main = do
  -- Ustawiamy wyjście na "NoBuffering", żeby prompty pojawiały się od razu.
  hSetBuffering stdout NoBuffering

  putStrLn "Skoczki."
  putStrLn "Notacja: E2-E3 (krok) lub E2-E4-G4 (wieloskok)."
  putStrLn "Ruchy: poziomo/pionowo; skok o 2 wymaga pionka na polu posrednim; brak bicia, brak skokow po przekatnej."
  putStrLn ""

  -- Opcja od razu z testami wieloskokow i logiki ruchu czyli numer 2
  putStrLn "Wybierz tryb:"
  putStrLn "1) Plansza standardowa"
  putStrLn "2) Plansza testowa (pod skoki i wieloskoki)"
  putStrLn "3) Plansza testowa (koniec gry - obie strony moga wygrac)"
  mode <- askMode

  let startBoard = case mode of
                     1 -> initialBoard
                     2 -> testBoard
                     3 -> endgameBoard
                     _ -> initialBoard

  putStrLn "Wybierz kolor w ktorym chcesz grac (B=Bialy, C=Czarny):"
  userColor <- askColor
  let aiColor = other userColor

  putStrLn ""
  putStrLn ("Gasz jako: " ++ show userColor)
  putStrLn ("AI gra jako: " ++ show aiColor)
  putStrLn ""

  -- Rozpoczynamy gre. Białe zawsze zaczynają.
  gameLoop startBoard Bialy userColor

-- Wczytanie trybu normalny lub testowy
askMode :: IO Int
askMode = do
  putStr "> "
  s <- getLine
  case s of
    "1" -> return 1
    "2" -> return 2
    "3" -> return 3
    _   -> putStrLn "Wpisz 1, 2 albo 3." >> askMode

-- gameLoop jest rekurencyjne i odpowiada za kolejne korki gry
-- Parametry:
--   b         - aktualna plansza
--   turn      - kto ma ruch teraz 
--   userColor - jaki kolor gra użytkownik (drugi kolor to AI)
gameLoop :: Board -> Color -> Color -> IO ()
gameLoop b turn userColor = do
  putStrLn ""
  putStrLn ("Tura: " ++ show turn)  -- show turn zamienia Biale/Czarne na String

  printBoard b -- wypisujemy jak wyglada plansza teraz

  -- tu sobie sprawdzamy czy ktos nie wygral bo po co dalej ewaluowac jak juz koniec jest
  if hasWon Bialy b then do
    putStrLn "Koniec gry. Wygrywa: Bialy"
  else if hasWon Czarny b then do
    putStrLn "Koniec gry. Wygrywa: Czarny"
  else do
    -- Jeśli to tura użytkownika, pytamy o ruch. Jeśli to tura AI, generujemy losowy ruch.
    mv <- if turn == userColor
          then askMovePrompt "Twoj ruch > "
          else do
            putStrLn "AI generuje ruch"
            generateAIMove turn b

    -- applyMove po pierwsze sprawdza czy ruch jest legal, a jesli tak to zwraca nowa plansze
    -- Zwraca Either String Board:
    --   Left "blad"  = błąd i opis co nie tak
    --   Right board  = otherwise nowa plansza
    case applyMove turn mv b of
      Left err -> do
        putStrLn ("Nielegalny ruch: " ++ err)
        -- wracamy znowu do tury tak jakby sie nie odbyla
        gameLoop b turn userColor

      Right b' -> do
        putStrLn ("Wykonano: " ++ showMove mv)

        -- jak ruch jest OK to idziemy do next tury no chyba ze gra sie skonczyla zwyciestwem w tym momencie
        if hasWon turn b'
          then do
            putStrLn ""
            printBoard b'
            putStrLn ("Koniec gry. Wygrywa: " ++ show turn)
          else gameLoop b' (other turn) userColor

-- Zmiana koloru na  przeciwny.
other :: Color -> Color
other Bialy = Czarny
other Czarny = Bialy

-- Zamiana koloru na pionek na planszy.
pieceOf :: Color -> Piece
pieceOf Bialy = PW
pieceOf Czarny = PB


-- funkcja sprawdzajaca warunek wygranej zgodnie z tym co na wikipedii jest
hasWon :: Color -> Board -> Bool
hasWon c b =
  let ps = allPiecesOf c b
      -- ps :: [Pos] - lista pozycji pionków
  in not (null ps) && all (\(_,r) -> r `elem` targetRows c) ps
  -- all f ps zwraca True, jeśli f jest True dla każdego elementu listy.

targetRows :: Color -> [Int]
targetRows Bialy = [7,8]
targetRows Czarny = [1,2]

-- [ (x,y) | y <- [1..8], x <- [1..8], warunek ]
-- Czyli: dla wszystkich y i x wybierz (x,y), jeśli spełnia warunek.
allPiecesOf :: Color -> Board -> [Pos]
allPiecesOf c b =
  [ (x,y)
  | y <- [1..8]
  , x <- [1..8]
  , getAtUnsafe (x,y) b == pieceOf c
  ]

askColor :: IO Color
askColor = do
  putStr "> "
  s <- getLine
  case s of
    "B" -> return Bialy
    "b" -> return Bialy
    "C" -> return Czarny
    "c" -> return Czarny
    _   -> putStrLn "Wpisz B albo C." >> askColor
  

askMovePrompt :: String -> IO Move
askMovePrompt prompt = do
  putStr prompt
  s <- getLine
  case parseMove s of
    --sprawdzamy czy ruch jest poprawny w sensie skoczkow
    Left e  -> putStrLn ("Blad formatu: " ++ e) >> askMovePrompt prompt
    Right m -> return m

-- generateAIMove: generuje losowy legalny ruch dla przeciwnika
generateAIMove :: Color -> Board -> IO Move
generateAIMove c b = do
  let pieces = allPiecesOf c b
  if null pieces
    then error "Brak pionkow do ruchu!"
    else do
      -- Probujemy az znajdziemy legalny ruch
      tryRandomMove c b pieces
  where
    tryRandomMove :: Color -> Board -> [Pos] -> IO Move
    tryRandomMove col brd pcs = do
      -- Bierz pierwszy pionek 
      let piece = head pcs
      -- Generuj losowy ruch dla tego pionka
      case generateMoveForPiece col brd piece of
        Just mv -> return mv
        Nothing -> 
          -- probuj nastepny az znajdziesz albo error
          if length pcs > 1
            then tryRandomMove col brd (tail pcs)
            else do
              putStrLn "AI nie moze znalezc legalnego ruchu!"
              error "Brak legalnych ruchow"

-- generateMoveForPiece: probuje wygenerowac ruch dla konkretnego pionka
-- Proste: probuj wszystkie kierunki (gora, dol, lewo, prawo) o 1 lub 2 pola
generateMoveForPiece :: Color -> Board -> Pos -> Maybe Move
generateMoveForPiece c b start =
  -- Probujemy wszystkie możliwe kierunki i dystanse
  let allMoves = [ Move [start, (x,y)] | x <- [1..8], y <- [1..8], (x,y) /= start ]
  in case filter (\mv -> case applyMove c mv b of; Right _ -> True; Left _ -> False) allMoves of
       (m:_) -> Just m
       []    -> Nothing

-- parseMove zamienia String na Move
-- Używamy Either w razie sytuacji gdyby trzeba bylo zwrocic opis bledu
parseMove :: String -> Either String Move
parseMove s =
  let parts = splitOn '-' (trim s)
      -- parts to lista kawałków np. ["E2","E4","G4"] zeby byl sensowny podzial
  in if length parts < 2
       then Left "ruch musi miec min. 2 pola, np. E2-E3"
       else do
         ps <- mapM parsePos parts
         --   parsePos :: String -> Either String Pos
         --   mapM łączy listę Either w jedno Either z listą wyników
         return (Move ps)

-- showMove robi odwrotnie do parseMove 
showMove :: Move -> String
showMove (Move ps) = joinWith "-" (map showPos ps)
-- map showPos ps zamienia [Pos] na [String]
-- joinWith "-" łączy ["E2","E4","G4"] w "E2-E4-G4"

-- lepiej jest operowac tylko na tuplach liczb wiec tu sobie zamieniamy np E2 na (5,2)
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
        --jak jest cos w rest no to user podal cos illegal wiec trzeba go poprawic
        else Left "pozycja ma format np. E2"

    _ -> Left "pozycja ma format np. E2"

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
--self explanatory
showPos :: Pos -> String
showPos (c,r) = [showCol c, showRow r]
--wsm reverse parseCol
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

----------------------------
---SKOKI I TYM PODOBNE------
----------------------------

-- applyMove:
--  1) sprawdza poprawność ruchu
--  2) jeśli poprawny no to zwraca nową planszę 
applyMove :: Color -> Move -> Board -> Either String Board
applyMove c (Move ps) b = do
  -- minimalnie 2 pozycje
  if length ps < 2 then Left "ruch za krotki" else Right ()

  -- nie mozna wyleciec poza plansze
  if any (not . inBounds) ps then Left "pozycja poza plansza" else Right ()

  let start = head ps
      end   = last ps 

  -- sprawdzamy czy na polu podanym jako pierwze jest faktycznie pionek tego gracza
  sp <- getAt start b
  if sp /= pieceOf c
    then Left "na polu startowym nie ma pionka gracza w ruchu"
    else Right ()

  -- Koniec musi byc empty
  ep <- getAt end b
  if ep /= Empty
    then Left "pole docelowe nie jest puste"
    else Right ()

  -- Jeśli user robi wieloskok to same skoki a nie ze skok + ruch
  if length ps > 2 && any (not . isJumpSegment) (zip ps (tail ps))
    then Left "ruch wieloodcinkowy musi skladac sie wylacznie ze skokow"
    else Right ()

  -- Walidujemy każdy segment ruchu
  validateSegments ps b

  -- Wykonanie: zdejmujemy pionek z startu i dajemy na koniec
  let b1 = setAt start Empty b
  return (setAt end (pieceOf c) b1)

-- validateSegments sprawdza po kolei każdy ruch wieloskoku
validateSegments :: [Pos] -> Board -> Either String ()
validateSegments [_] _ = Right ()  -- jeśli została 1 pozycja, to koniec walidacji
validateSegments (a:b:rest) brd = do
  -- Pole koncowe musi być puste
  dest <- getAt b brd
  if dest /= Empty then Left "wejscie na zajete pole" else Right ()

  -- Różnice współrzędnych:
  -- dc = delta kolumny, dr = delta wiersza
  let dc = fst b - fst a
      dr = snd b - snd a
      adc = abs dc
      adr = abs dr

  -- krok  o 1
  let isStep = (adc == 1 && adr == 0) || (adc == 0 && adr == 1)

  -- skok  o 2
  let isJump = (adc == 2 && adr == 0) || (adc == 0 && adr == 2)

  if not (isStep || isJump) --jak np przekatna damy to zle
    then Left "dozwolone sa tylko ruchy poziome/pionowe o 1 lub skoki o 2"
    else Right ()

  if isJump
    then do
     --tu sie odbywa walidacja czy pole w srodku jest zajete, nie mozna przeskoczyc "powietrza"
      let mid = (fst a + dc `div` 2, snd a + dr `div` 2)
      mp <- getAt mid brd
      if mp == Empty
        then Left "skok wymaga pionka na polu posrednim"
        else validateSegments (b:rest) brd
    else
      validateSegments (b:rest) brd

-- walidacja czy ruch jest skokiem
isJumpSegment :: (Pos, Pos) -> Bool
isJumpSegment (a,b) =
  let dc = abs (fst b - fst a)
      dr = abs (snd b - snd a)
  in (dc == 2 && dr == 0) || (dc == 0 && dr == 2)

-------------
-- PLANSZA --
-------------

-- Czy współrzędne są w 1..8.
inBounds :: Pos -> Bool
inBounds (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

-- getAt: bezpieczny odczyt pola z walidacja granic, latwiej bylo wydzielic do osobnej funkcji brak walidacji granic
getAt :: Pos -> Board -> Either String Piece
getAt p b =
  if inBounds p then Right (getAtUnsafe p b)
  else Left "poza plansza"

-- getAtUnsafe: szybki odczyt pola (zakladamy, ze p jest w bounds)
getAtUnsafe :: Pos -> Board -> Piece
getAtUnsafe (x,y) b =
  (b !! (y-1)) !! (x-1)  -- odejmujemy 1, bo w Pos mamy 1..8, a listy są 0..7

-- setAt: ustawia wartość pola (x,y) na nasze "v" i zwraca nowa wersje planszy
setAt :: Pos -> Piece -> Board -> Board
setAt (x,y) v b =
  let row  = b !! (y-1)
      row' = take (x-1) row ++ [v] ++ drop x row
      -- take (x-1) bierze elementy przed kolumną,
      -- [v] wstawia nową wartość,
      -- drop x pomija element na pozycji (x-1) i bierze resztę.
  in take (y-1) b ++ [row'] ++ drop y b
     -- analogicznie podmieniamy cały wiersz w planszy

-----------------------
-- OUTPUT PLANSZY------
-----------------------

printBoard :: Board -> IO ()
printBoard b = do
  putStrLn "    A B C D E F G H"

-- tu zamieniamy kolejnosc na liczoną od 8 do 1
  mapM_ putStrLn (zipWith renderRow [8,7..1] (reverse b))

  putStrLn "    A B C D E F G H"
  where
    renderRow :: Int -> [Piece] -> String
    renderRow r row =
      pad2 (show r) ++ "  "
      ++ joinWith " " (map (\p -> [renderPiece p]) row)
      -- map (\p -> [renderPiece p]) row zamienia [Piece] na ["B, C" itd]
      ++ "  " ++ pad2 (show r)
      

renderPiece :: Piece -> Char
renderPiece p =
  case p of
    PW    -> 'B'
    PB    -> 'C'
    Empty -> '.'

pad2 :: String -> String
pad2 s = if length s == 1 then ' ' : s else s

-------------------------
-- POCZATKOWA PLANSZA I TESTY --
-------------------------

initialBoard :: Board
initialBoard =
  [ row 1, row 2, row 3, row 4, row 5, row 6, row 7, row 8 ]
  where
    row r
      | r <= 2 = replicate 8 PW
      | r >= 7 = replicate 8 PB
      | otherwise = replicate 8 Empty
-- Plansza testowa do sprawdzenia wieloskoków:
-- Legenda:
--  - W pionek (biały) ma wykonać wieloskok: E4-E6-G6
--    bo:
--      E5 zajęte (skok w górę)
--      F6 zajęte (skok w prawo)
--  - Do tego parę pionków czarnych, żeby plansza nie była pusta.
testBoard :: Board
testBoard =
  placeMany emptyBoard
    [ ((5,4), PW)  -- E4: biały do testów
    , ((5,5), PB)  -- E5: pionek do przeskoczenia
    , ((6,6), PW)  -- F6: pionek do przeskoczenia
    , ((2,2), PB)  -- B2: coś czarnego
    , ((7,7), PW)  -- G7: coś białego
    ]
-- Plansza testowa do sprawdzenia warunku zwyciestwa:
-- Biale maja wiekszosc pionkow na rzedach 7-8, ale jeden na rzedzie 6
-- Czarne maja wiekszosc pionkow na rzedach 1-2, ale jeden na rzedzie 3
-- Po ruchu jednego pionka do strefy zwyciestwa, gracz wygrywa.
endgameBoard :: Board
endgameBoard =
  placeMany emptyBoard
    [ ((1,7), PW)  -- A7: bialy na polu zwyciestwa
    , ((3,8), PW)  -- C8: bialy na polu zwyciestwa
    , ((5,6), PW)  -- E6: bialy do przesuniecia (moze isc na E7 lub E8)
    , ((1,1), PB)  -- A1: czarny na polu zwyciestwa
    , ((4,2), PB)  -- D2: czarny na polu zwyciestwa
    , ((7,3), PB)  -- G3: czarny do przesuniecia (moze isc na G2 lub G1)
    ]
emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Empty)

-- placeMany ustawia wiele pionków naraz (użyteczne do testBoard).
placeMany :: Board -> [(Pos, Piece)] -> Board
placeMany b xs = foldl (\acc (p,pc) -> setAt p pc acc) b xs
-- foldl:
--  bierze akumulator (acc = plansza) i po kolei aplikuje zmiany

-- funkcje pomocnicze do stringow
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

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs