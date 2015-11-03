import qualified Data.Set as Set
import Data.List
import AlfaCierre

type Atributo = Set.Set Char
type Dependencias = (Atributo, Atributo)

claves :: Atributo -> Set.Set Dependencias -> Set.Set Atributo
claves r f = claves' r f (map Set.fromList (subsequences (Set.toList curr))) (Set.size curr) 1 Set.empty 
                where curr = (Set.fold Set.union Set.empty (Set.map fst f))


claves' :: Atributo -> Set.Set Dependencias -> [Atributo] -> Int -> Int -> Set.Set Atributo -> Set.Set Atributo
claves' r f curr s_curr skey keys =  if skey > s_curr
                                     then keys
                                     else if length curr' <= 0
                                          then claves' r f curr s_curr (skey+1) keys
                                          else claves' r f tot s_curr (skey+1) (Set.union keys ks)
                                            where curr' = filter (\x -> Set.size x == skey) curr 
                                                  (tot, ks) = foreach r f curr' curr Set.empty

foreach :: Atributo -> Set.Set Dependencias -> [Atributo] -> [Atributo] -> Set.Set Atributo -> ([Atributo], Set.Set Atributo)
foreach r f [] total keys = (total, keys)
foreach r f (c:curr) total keys = if  cierre c f == r
                                  then foreach r f l t (Set.insert c keys)
                                  else foreach r f curr total keys
                                     where l = filter (\x -> not (Set.isSubsetOf c x)) curr
                                           t = filter (\x -> not (Set.isSubsetOf c x)) total
{-
claves (R,F)

    keys = empty
    
    current = P({determinantes de F})
    
    size_current = size ({determinantes de F})
    
    skey = 1
    
    while (skey <= size_current){
    
        foreach x e current / size(x) = skey{
        
            if cierre(x,F) = R then { insert x keys,
                                      foreach y e current{
                                            if (x--contenido igual en--y then delete y current
                                      }
                                    }
        }
        
        skey++
    }
-}

ar :: Atributo
ar = Set.fromList "ABCDEGH"

a0dep :: Set.Set Dependencias
a0dep = Set.fromList [(Set.fromList ['A'], Set.fromList "BC"), (Set.fromList ['C'], Set.fromList ['D']), (Set.fromList ['D'], Set.fromList ['G']), (Set.fromList ['H'], Set.fromList ['E']), (Set.fromList ['E'], Set.fromList ['A']), (Set.fromList ['E'], Set.fromList ['H'])]


a1dep :: Set.Set Dependencias
a1dep = Set.fromList [(Set.fromList ['A'], Set.fromList ['C']), (Set.fromList ['B'], Set.fromList ['D']), (Set.fromList "BC", Set.fromList ['E']), (Set.fromList "BC", Set.fromList ['H']), (Set.fromList "GH", Set.fromList ['E']), (Set.fromList ['D'], Set.fromList ['A'])]

a2dep :: Set.Set Dependencias
a2dep = Set.fromList [(Set.fromList ['B'], Set.fromList ['C']), (Set.fromList ['D'], Set.fromList ['A']), (Set.fromList ['E'], Set.fromList ['H']), (Set.fromList ['C'], Set.fromList ['E']), (Set.fromList ['B'], Set.fromList ['G'])]

br :: Atributo
br = Set.fromList "ABCDEFGHIJ"

bdep :: Set.Set Dependencias
bdep = Set.fromList [(Set.fromList "AB", Set.fromList ['C']), (Set.fromList "BD", Set.fromList "EF"), (Set.fromList "AD", Set.fromList "GH"), (Set.fromList ['A'], Set.fromList ['I']), (Set.fromList ['H'], Set.fromList ['J'])]
