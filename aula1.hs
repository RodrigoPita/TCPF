safediv :: Integer -> (Integer -> Maybe Integer)
safediv n 0 = Nothing
safediv n m = Just(div n m)

data Talvez a = Nada | DeFato a
    deriving (Show, Eq)

instance Functor Talvez
    where
        fmap f Nada = Nada
        fmap f DeFato x = DeFato (f x)

data ArvBin a = No a (ArvBin a) (ArvBin a) | Folha
    deriving Show

t = No 13 Folha (No 25 Folha Folha)

instance Functor ArvBin
    where
        fmap f Folha =  Folha
        fmap f (No x arv_esq arv_dir) = No (f x) --- completar! 
