newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader ra)= Reader $ \r -> f(ra r)

instance Applicative (Reader r) where
    pure x = Reader $ \_ -> x
  
    liftA2 f (Reader ra) (Reader rb) = Reader $ \r -> f (ra r)(rb r)

instance Monad (Reader r) where
    (Reader ra) >>= f = Reader $ \r -> runReader (f(ra r)) r


ask :: Reader r r
ask = Reader $ \r->r  

asks :: (r -> a) -> Reader r a
asks f = Reader $ \r -> f r  

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader $ \r -> ra(f r)
  

data BankConfig = BankConfig {
    interestRate :: Double,
    transactionFee :: Int,
    minimumBalance :: Int
} deriving (Show)

data Account = Account {
    accountId :: String,
    balance :: Int
} deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
    rate <- asks interestRate
    return $ round (fromIntegral(balance acc) * rate)

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
    fee <- asks transactionFee
    return $ acc {balance = balance acc - fee}

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
    minBal <- asks minimumBalance
    return $ balance acc >= minBal

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
    updatedAcc <- applyTransactionFee acc
    interest <- calculateInterest acc
    meetsMin <- checkMinimumBalance acc
    return (updatedAcc, interest, meetsMin)