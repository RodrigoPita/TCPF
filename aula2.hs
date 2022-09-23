data ArvBin a = No a (ArvBin a) (ArvBin a) | Folha
  deriving Show

t = No 13 Folha (No 25 Folha Folha)

instance Functor ArvBin
  where
    fmap f Folha = Folha
    fmap f (No x arv_esq arv_dir)
      = No (f x) (fmap f arv_esq) (fmap f arv_dir)

-- funtores em haskell não precisam ser funtores no sentido
-- matemático!! exemplo do Hashi:

data Hashi a = HashiNo a (Hashi a) (Hashi a) | HashiFolha
  deriving Show

instance Functor Hashi where
  fmap f HashiFolha = HashiFolha
  fmap f (HashiNo x arv_esq arv_dir) = HashiFolha
  -- quebra a lei de que um funtor (matemático) deve mapear identidades para identidades

-- Desafio (parte b): árvores onde cada nó pode ter
-- quantidade qualquer de filhos

-- continuação do "minucurso de Haskell"

-- exemplo muito bobo
g :: (Eq a, Num a) => a -> a -> a
g x 0 = x
g x y = x + y

h :: (Eq a, Num a) => a -> a -> a
h _ 0 = 0
h x y = x * y

data Natural = Zero | Suc Natural
  deriving (Show, Eq)

um = Suc Zero
dois = Suc um

soma :: Natural -> Natural -> Natural
soma n Zero    = n
soma n (Suc m) = soma (Suc n) m
-- alternativa
-- soma n (Suc m) = Suc (soma n m)

produto :: Natural -> Natural -> Natural
produto n Zero    = Zero
produto n (Suc m) = soma (produto n m) n

-- Exercí­cio: potência

i :: Integer -> Integer
i 0 = 15
i x = undefined -- usar undefined = pedir pro Haskell "esperar aÃ­"

-- haskell Ã© lazy!
números   = [0..]
quadrados = fmap (\x -> x^2) números

-- exemplo que ainda não entendemos (Rufino), para motivar:
primos = crivo [2..] where
  crivo (t:xs) = t : (crivo [x | x <- xs, x `mod` t > 0])