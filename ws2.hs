import System.IO (stdin, hIsEOF)

data Token = Add
           | Sub
           | Mul
           | Div
           | Num Int
           deriving Show

eval xs = go [] xs where
    go (r:l:ys) (Add:zs) = go (l + r : ys) zs
    go (r:l:ys) (Sub:zs) = go (l - r : ys) zs
    go (r:l:ys) (Mul:zs) = go (l * r : ys) zs
    go (r:l:ys) (Div:zs) = case r of
                            0 -> Left "division by zero"
                            _ -> go (l `quot` r : ys) zs
    go ys ((Num n):zs)   = go (n:ys) zs
    go [x] [] = Right x

tokenize str = foldr step (Right []) $ words str where
    step elm (Right acc) = case elm of
        "+" -> Right $ Add : acc
        "-" -> Right $ Sub : acc
        "*" -> Right $ Mul : acc
        "/" -> Right $ Div : acc
        _   -> case reads elm of
            [(n, [])] -> Right $ Num n : acc
            [(n, xs)] -> case step xs (Right acc) of
                Left  err  -> Left err
                Right toks -> Right $ Num n : toks ++ acc
            _         -> Left $ "not an operator or number: " ++ show elm
    step _   (Left  err) = Left err

main :: IO ()
main = do
    line <- getLine
    let tokres = tokenize line
    case tokres of 
        Left err -> putStrLn err
        Right toks -> case eval toks of
            Left err -> putStrLn err
            Right val -> putStrLn $ show val
    eof <- hIsEOF stdin
    if eof then
        putStrLn "Bye!"
    else
        main

