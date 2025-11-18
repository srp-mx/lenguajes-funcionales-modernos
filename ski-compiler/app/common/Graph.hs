module Graph (
    Graph(Graph),
    stronglyConnected,
    converse,
    acyclicOrder,
    acyclicSort,
    stronglyConnectedComponents,
    verticesInCycles,
) where

import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Hashable(Hashable)

data Graph a = Graph [a] (a -> Set.HashSet a)

-- |Obtiene el conjunto de vértices fuertemente conexos con un conjunto de
--  vértices inicial
stronglyConnected :: (Eq a, Hashable a) => Graph a -> [a] -> Set.HashSet a
stronglyConnected (Graph _ neigh) = dfs Set.empty
    where dfs seen [] = seen
          dfs seen (s:stack)
            | s `Set.member` seen = dfs seen stack
            | otherwise =
                let
                    seen' = Set.insert s seen
                    discovered = Set.difference (neigh s) seen
                    stack' = stack ++ Set.toList discovered
                in dfs seen' stack'

-- |La gráfica conversa es la gráfica con los arcos volteados
converse :: (Eq a, Hashable a) => Graph a -> Graph a
converse (Graph vs neigh) = Graph vs neigh'
    where joinRevU u acc =
            let targets = Set.toList (neigh u)
                su = Set.singleton u
            in foldr (\v m -> Map.insertWith Set.union v su m) acc targets
          revMap = foldr joinRevU Map.empty vs
          neigh' v = Map.findWithDefault Set.empty v revMap

-- |Orden acíclico: si u->v, entonces u aparece antes que v. Asume que la
--  gráfica es acíclica, pero si no lo es entonces es fácil detectar un ciclo
--  al notar que algún vértice apunta a otro con un índice menor.
acyclicOrder :: (Eq a, Hashable a) => Graph a -> Map.HashMap a Int
acyclicOrder g = Map.fromList (zip (acyclicSort g) [0..])

-- |Ordenamiento acíclico. Asume que la gráfica es acíclica.
acyclicSort :: (Eq a, Hashable a) => Graph a -> [a]
acyclicSort (Graph vs neigh) = dfsAll vs Set.empty []
  where dfsAll [] _ acc = acc
        dfsAll (v:rest) seen acc
          | v `Set.member` seen = dfsAll rest seen acc
          | otherwise = dfsAll rest seen' acc'
          where (seen', acc') = dfs v seen acc
        dfs v seen acc
          | v `Set.member` seen = (seen, acc)
          | otherwise =
              let seen' = Set.insert v seen
                  (seenFinal, accFinal) =
                    foldr (\u (s,a) -> dfs u s a) (seen', acc) (Set.toList (neigh v))
              in (seenFinal, v:accFinal)

-- |Búsqueda por profundidad auxiliar, con conjunto de visitados "global"
subdfs :: (Eq a, Hashable a) => Graph a -> a -> Set.HashSet a -> (Set.HashSet a, Set.HashSet a)
subdfs (Graph _ neigh) v seen
    | v `Set.member` seen = (Set.empty, seen)
    | otherwise =
        let aux stack comp seen' = case stack of
                [] -> (comp, seen')
                x:xs ->
                    if x `Set.member` seen'
                    then aux xs comp seen'
                    else let seen'' = Set.insert x seen'
                             comp'  = Set.insert x comp
                             stack' = xs ++ Set.toList (neigh x)
                         in aux stack' comp' seen''
      in aux [v] Set.empty seen

-- |Descomposición en componentes fuertemente conexas con Kosaraju.
stronglyConnectedComponents :: (Eq a, Hashable a) => Graph a -> [Set.HashSet a]
stronglyConnectedComponents g =
    let order = acyclicSort g
        gRev = converse g
        aux [] _ = []
        aux (v:rest) seen
            | v `Set.member` seen = aux rest seen
            | otherwise =
                let (comp, seen') = subdfs gRev v seen
                in comp : aux rest seen'
    in aux order Set.empty

-- |Da todos los vértices que se encuentran en algún ciclo (o lazo)
verticesInCycles :: (Eq a, Hashable a) => Graph a -> Set.HashSet a
verticesInCycles g@(Graph _ neigh) =
    let comps = stronglyConnectedComponents g
        cycComps = filter isCycle comps
    in foldr Set.union Set.empty cycComps
  where
    isCycle comp
      | Set.size comp > 1 = True
      | otherwise =
          let v = head (Set.toList comp)
          in v `Set.member` neigh v
