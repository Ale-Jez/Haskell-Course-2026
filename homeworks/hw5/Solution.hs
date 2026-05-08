import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
    deriving (Show, Eq)

execInstr :: Instr -> State [Int] ()
execInstr instr = modify $ \stack -> case (instr, stack) of
    (PUSH n, xs)   -> n : xs
    (POP, _:xs)    -> xs
    (DUP, x:xs)    -> x : x : xs
    (SWAP, x:y:xs) -> y : x : xs
    (ADD, x:y:xs)  -> (x + y) : xs
    (MUL, x:y:xs)  -> (x * y) : xs
    (NEG, x:xs)    -> (-x) : xs
    (_, xs)        -> xs

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg instrs = execState (execProg instrs) []

data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr
    | Seq  Expr Expr
    deriving (Show)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n
eval (Var x) = do
    env <- get
    return $ env Map.! x 
eval (Add e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return (v1 + v2)
eval (Mul e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return (v1 * v2)
eval (Neg e) = do
    v <- eval e
    return (-v)
eval (Assign x e) = do
    v <- eval e
    modify (Map.insert x v)
    return v
eval (Seq e1 e2) = do
    _ <- eval e1
    eval e2



runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just dist -> return dist
        Nothing -> do
            dist <- compute
            modify (Map.insert (i, j) dist)
            return dist
    where
        compute
            | i == 0 = return j
            | j == 0 = return i
            | xs !! (i - 1) == ys !! (j - 1) = editDistM xs ys (i - 1) (j - 1)
            | otherwise = do
                del <- editDistM xs ys (i - 1) j
                ins <- editDistM xs ys i (j - 1)
                sub <- editDistM xs ys (i - 1) (j - 1)
                return (1 + minimum [del, ins, sub])

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty

data GameState = GameState
    { position :: Int
    , energy   :: Int
    , score    :: Int
    } deriving (Show)

type AdventureGame a = StateT GameState IO a


movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
    modify $ \st -> st 
        { position = position st + roll
        , energy   = energy st - 1 
        }
    return roll



makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    liftIO $ putStrLn "\n*** DECISION POINT ***"
    liftIO $ putStrLn "You reach a crossroad. Which path will you take?"
    choice <- liftIO $ getPlayerChoice options
    return choice

handleLocation :: AdventureGame Bool
handleLocation = do
    st <- get
    let pos = position st
    case pos of
        3 -> do
            liftIO $ putStrLn "Ooo! You found a small treasure cache! (+10 Score)"
            modify $ \s -> s { score = score s + 10 }
            return False
        5 -> do
            liftIO $ putStrLn "SNAP! You stepped on a hidden trap! (-5 Score)"
            modify $ \s -> s { score = max 0 (score s - 5) }
            return False
        8 -> do
            choice <- makeDecision ["Take the dark cave", "Walk around the mountain"]
            if choice == "Take the dark cave"
                then do
                    liftIO $ putStrLn "The cave was fast but terrifying! (+3 spaces, -2 energy)"
                    modify $ \s -> s { position = position s + 3, energy = energy s - 2 }
                else do
                    liftIO $ putStrLn "A safe but very long walk. (-1 energy)"
                    modify $ \s -> s { energy = energy s - 1 }
            return False
        12 -> do
            liftIO $ putStrLn "A massive fallen tree blocks your path. (-2 energy to climb over)"
            modify $ \s -> s { energy = energy s - 2 }
            return False
        _ | pos >= 15 -> do
            liftIO $ putStrLn "\n*** CONGRATULATIONS! You reached the main treasure! ***"
            return True
        _ -> do
            liftIO $ putStrLn "The path is clear. You march forward."
            return False

playTurn :: AdventureGame Bool
playTurn = do
    roll <- liftIO getDiceRoll
    _ <- movePlayer roll
    st <- get
    if energy st <= 0
        then do
            liftIO $ putStrLn "\nYou collapsed from exhaustion! GAME OVER."
            return True
        else do
            reachedGoal <- handleLocation
            return reachedGoal



playGame :: AdventureGame ()
playGame = do
    st <- get
    liftIO $ displayGameState st
    ended <- playTurn
    if ended
        then liftIO $ putStrLn "Thanks for playing!"
        else playGame



getDiceRoll :: IO Int
getDiceRoll = do
    putStr "Roll the dice (enter a number 1-6): "
    input <- getLine
    case reads input of
        [(n, "")] | n >= 1 && n <= 6 -> return n
        _ -> do
            putStrLn ">> Invalid roll! Please enter a valid number between 1 and 6."
            getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState st = do
    putStrLn "\n=================================================="
    putStrLn $ "    Position: " ++ show (position st)
    putStrLn $ "    Energy:   " ++ show (energy st)
    putStrLn $ "    Score:    " ++ show (score st)
    putStrLn "=================================================="



getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose an option:"
    let numberedOptions = zip [1..] options
    mapM_ (\(i, opt) -> putStrLn $ "  " ++ show (i :: Int) ++ ". " ++ opt) numberedOptions
    putStr "Enter your choice (number): "
    input <- getLine
    case reads input of
        [(n, "")] | n >= 1 && n <= length options -> return (options !! (n - 1))
        _ -> do
            putStrLn $ ">> Invalid choice! Please enter a number between 1 and " ++ show (length options) ++ ".\n"
            getPlayerChoice options