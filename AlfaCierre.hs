module AlfaCierre (cierre) where

import qualified Data.Set as Set

type Atributo = Set.Set Char
type Dependencias = (Atributo, Atributo)

cierre :: Atributo -> Set.Set Dependencias -> Atributo
cierre alpha dep = let check = checkDF alpha (Set.elems dep)
                   in cierre' check dep alpha

cierre' :: Atributo -> Set.Set Dependencias -> Atributo -> Atributo
cierre' alpha dep alpha' = if alpha == alpha' --(Set.isSubsetOf alpha alpha') && (Set.isSubsetOf alpha' alpha)
                           then alpha
                           else cierre' check dep alpha
                                where check = checkDF alpha (Set.elems dep)

checkDF :: Atributo -> [Dependencias] -> Atributo
checkDF alpha []     = alpha
checkDF alpha (x:xs) = if Set.isSubsetOf (fst x) alpha
                         then checkDF (Set.union (snd x) alpha) xs
                         else checkDF alpha xs


a :: Atributo
a = Set.insert 'A' (Set.insert 'G' (Set.empty))

adep :: Set.Set Dependencias
adep = Set.fromList [(Set.fromList ['A'], Set.fromList ['B']), (Set.fromList ['A'], Set.fromList ['C']), (Set.fromList ['C', 'G'], Set.fromList ['H']), (Set.fromList ['C', 'G'], Set.fromList ['I']), (Set.fromList ['B'], Set.fromList ['H'])]

b :: Atributo
b = Set.fromList "ABCDEFGHIJ"

bdep :: Set.Set Dependencias
bdep = Set.fromList [(Set.fromList "AB", Set.fromList "C"), (Set.fromList "BD", Set.fromList "EF"), (Set.fromList "AD", Set.fromList "GH"), (Set.fromList "A", Set.fromList "I"), (Set.fromList "H", Set.fromList "J")]

a1dep = Set.fromList [(Set.fromList "AB",Set.fromList "C"), (Set.fromList "BD",Set.fromList "EF"), (Set.fromList "AD", Set.fromList "GH"), (Set.fromList "A", Set.fromList "I"), (Set.fromList "H", Set.fromList "J")]
a1 = Set.fromList "BD"
