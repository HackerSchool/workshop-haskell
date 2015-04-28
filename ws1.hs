-- Tipo de dados que representa os elementos da expressão
data Token = Add
           | Sub
           | Mul
           | Div
           | Num Int
           deriving Show

eval :: [Token]  -- A expressão
     -> Int      -- O resultado
eval xs = go [] xs where
    go :: [Int]    -- A nossa pilha para avaliar a expressão
       -> [Token]  -- A expressão
       -> Int      -- O resultado
    -- Casos simples (temos uma função no input e dois valores na pilha
    go (r:l:ys) (Add:zs) = go (l + r : ys) zs
    go (r:l:ys) (Sub:zs) = go (l - r : ys) zs
    go (r:l:ys) (Mul:zs) = go (l * r : ys) zs
    go (r:l:ys) (Div:zs) = go (l `quot` r : ys) zs
    -- Um número no input é colocado na pilha
    go ys ((Num n):zs)   = go (n:ys) zs
    -- Se já não temos input e temos um valor na pilha então é o resultado final
    go [x] [] = x
    -- Tratar casos de erro
    go []  _ = error "Operadores a mais"
    go  _ [] = error "Números a mais"

tokenize :: String   -- A expressão como uma string
         -> [Token]  -- A expressão partida em bocadinhos convertidos em Tokens
tokenize str = foldr step [] $ words str where
    -- Pega num pedacinho do input, converte-o para token e acumula-o com os tokens já lidos
    step :: String
         -> [Token]
         -> [Token]
    step elm acc = case elm of
        "+" -> Add : acc
        "-" -> Sub : acc
        "*" -> Mul : acc
        "/" -> Div : acc
        _   -> Num (read elm) : acc

main :: IO ()
main =    interact $ unlines . map processLine . lines where
    -- 1.                                     ^ parte o input em linhas
    -- 2.                   ^ por cada linha aplica a função processLine
    -- 3.         ^ pega na lista de resultados e junta-os numa sequência de linhas
    -- 4. ^ pega na função após o $, aplica-a ao input do programa e imprime o resultado
    -- Pega numa linha de texto, parte-a em tokens, avalia-a e converte o resultado em texto
    processLine :: String
                -> String
    processLine = show . eval . tokenize

