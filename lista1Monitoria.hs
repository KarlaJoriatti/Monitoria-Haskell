
main = do
         let
           a = somatorio 3 3
           b = calculaPA 7 2
           c = juros 1250 0.0155 7
           d = decaimento 2.5 2016
         putStrLn ("somatorio 3 3 = "++show(a))
         putStrLn ("calculaPA 7 2 = "++show(b))
         putStrLn ("juros 1250 0.0155 7 = "++show(c))
         putStrLn ("decaimento 2.5 2016 = "++show(d))

somatorio :: Int -> Int -> Int
somatorio _ 0 = 1 -- caso de parada
somatorio m n = m^n + somatorio m (n-1)

calculaPA :: Int -> Int -> Int
calculaPA n r = calculaPAaux n r 1
calculaPAaux 1 r m = m -- caso de parada
calculaPAaux n r m = calculaPAaux (n-1) r (m+r)

juros :: Float -> Float -> Int -> Float
juros valor percento 0 = valor -- caso de parada
juros valor percento anos = juros (valor*percento+valor) percento (anos-1)

decaimento :: Float -> Float -> Int
decaimento massa inicio = if massa < 1 then
                             truncate(inicio)
                          else
                             decaimento (massa/2) (inicio+5.26)
