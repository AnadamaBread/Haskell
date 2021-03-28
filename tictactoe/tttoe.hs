module Main where
    import System.Environment
    import Data.List

    data Move = X | O
    data Cell = Occupied Move | Empty
    data CellTrans = Fail String [Cell] | Success [Cell]

    instance Show Cell where
        show (Occupied X) = "X"
        show (Occupied O) = "O"
        show Empty        = " "

    instance Show Move where
        show X = "X"
        show O = "O"

    instance Eq Cell where
        Occupied X == Occupied X = True
        Occupied O == Occupied O = True
        Empty == Empty           = True
        _==_                     = False

    nextMove :: Move -> Move
    nextMove X = O
    nextMove O = X

    renderRow :: [Cell] -> String
    renderRow row = intercalate " | " $ fmap show row

    dLine :: String
    dLine = "----------"    

    renderBoard :: [Cell] -> IO ()
    renderBoard board = do
        putStrLn $ renderRow firstRow
        putStrLn dLine
        putStrLn $ renderRow secondRow
        putStrLn dLine
        putStrLn $ renderRow thirdRow
        where 
            firstRow = take 3 board
            secondRow = drop 3 . take 6 $ board 
            thirdRow = drop 6 board
        
    getBoardIndex :: String -> Maybe Int
    getBoardIndex "A1" = Just 0
    getBoardIndex "A2" = Just 1
    getBoardIndex "A3" = Just 2
    getBoardIndex "B1" = Just 3
    getBoardIndex "B2" = Just 4
    getBoardIndex "B3" = Just 5
    getBoardIndex "C1" = Just 6
    getBoardIndex "C2" = Just 7
    getBoardIndex "C3" = Just 8
    getBoardIndex _ = Nothing

    verifyIfFree :: [Cell] -> Int -> Maybe Int
    verifyIfFree board ix = if board !! ix == Empty then Just ix else Nothing 

    assignCell :: String -> Move -> [Cell] -> CellTrans
    assignCell location move board = 
        case getBoardIndex location >>= verifyIfFree board of
            Nothing -> Fail "Invalid Move" board 
            Just i -> Success ((take i board) ++ [Occupied move] ++ (drop(i+1) board))
    
    isWinner :: Move -> [Cell] -> Bool 
    isWinner move board =
        or [
            -- check rows
            board !! 0 == (Occupied move) && board !! 1 == (Occupied move) && board !! 2 == (Occupied move),
            board !! 3 == (Occupied move) && board !! 4 == (Occupied move) && board !! 5 == (Occupied move),
            board !! 6 == (Occupied move) && board !! 7 == (Occupied move) && board !! 8 == (Occupied move),
            -- check columns
            board !! 0 == (Occupied move) && board !! 3 == (Occupied move) && board !! 6 == (Occupied move),
            board !! 1 == (Occupied move) && board !! 4 == (Occupied move) && board !! 7 == (Occupied move),
            board !! 2 == (Occupied move) && board !! 5 == (Occupied move) && board !! 8 == (Occupied move),
            -- check diagonals 
            board !! 0 == (Occupied move) && board !! 4 == (Occupied move) && board !! 8 == (Occupied move),
            board !! 6 == (Occupied move) && board !! 4 == (Occupied move) && board !! 2 == (Occupied move)
        ]

    playRound :: Move -> [Cell] -> IO ()
    playRound move board = do
        putStrLn $ (show move) ++ " 's turn."
        putStrLn $ "Pick a Cell from A1 to C3."
        renderBoard board 
        putStr "\nInput: "
        cell <- getLine 
        case assignCell cell move board of
            Fail err board -> do
                putStrLn err
                playRound move board
            Success newBoard -> do
                if isWinner move newBoard then do
                    putStrLn $("Winner! : " ++ (show move) ++ " has won!")
                    renderBoard newBoard
                    return ()
                else playRound (nextMove move) newBoard

    main :: IO()
    main = do
        putStrLn $ "Game Start!"
        let newBoard = replicate 9 Empty
        playRound X newBoard







