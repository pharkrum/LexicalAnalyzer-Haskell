main = do
    let pilha = ['E']
    print $ analisador(pilha,"(0+l.x)+x ")

troca:: Int -> [a] -> [a] -> [a]
troca i y novoValor = 
    if and [i>0, i<= length y]
    then let (ys,zs) = splitAt (i-1) y in  ys ++ novoValor ++ tail zs
    else y

analisador :: ([Char],[Char]) -> Bool
analisador ("","") = True
analisador ("", _) = False
analisador ((x:xs),(y:ys)) = analisador(gramatica (x:xs) (y:ys))

gramatica :: [Char] -> [Char] -> ([Char],[Char])
gramatica (x:xs) (y:ys)
    | x == y = (xs,ys)
    | x == 'E' && y == '0' = (troca 1 (x:xs) ['0'] ,(y:ys))
    | x == 'E' && y == '1' = (troca 1 (x:xs) ['1'] ,(y:ys))
    | x == 'E' && y == 'x' = (troca 1 (x:xs) ['x'] ,(y:ys))
    | x == 'E' && y == 'y' = (troca 1 (x:xs) ['y'] ,(y:ys))
    | x == 'E' && y == '(' = (troca 1 (x:xs) ['(','E','A','E',')'] ,(y:ys))
    | x == 'A' && y == '+' = (troca 1 (x:xs) ['+'] ,(y:ys))
    | x == 'A' && y == '-' = (troca 1 (x:xs) ['-'] ,(y:ys))
    | x == 'A' && y == '*' = (troca 1 (x:xs) ['*'] ,(y:ys))
    | x == 'A' && y == '/' = (troca 1 (x:xs) ['/'] ,(y:ys))
    | otherwise = ([],y:ys)


