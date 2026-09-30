module Main (main) where

import qualified Data.Matrix as Matrix
import Data.Matrix (Matrix)

import qualified Data.Vector as Vector
import Data.Vector (Vector, (!))

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import qualified Data.List as List
import Control.Applicative (asum)
import Data.Function ((&))



{----------- Modelos -----------}

-- Quantidade de prédios visiveis em cada sentido.
-- Da esquerda para direita e de cima para baixo.
data ViewReq = ViewReq {
  _rows :: Vector (Int, Int),
  _cols :: Vector (Int, Int)
}

-- Retorna os requisitos de visibilidade para uma linha.
rowViewReq :: Options -> Int -> (Int, Int)
rowViewReq opts i = (_rows $ _viewReq opts) ! (i - 1)

-- Retorna os requisitos de visibilidade para uma coluna.
colViewReq :: Options -> Int -> (Int, Int)
colViewReq opts j = (_cols $ _viewReq opts) ! (j - 1)

-- Representa o tabuleiro do jogo junto de suas configurações.
data Options = Options {
  _n :: Int,
  _maxHeight :: Int,
  _viewReq :: ViewReq,
  _checkDiags :: Bool
}

-- Representa um tabuleiro com os estados possíveis de cada um dos quadrados.
type Board = Matrix IntSet

-- Representa um tabuleiro preenchido, ou seja, com todos os estados colapsados.
type FilledBoard = Matrix Int 

-- Representa um tabuleiro completo, ou seja, o puzzle foi resolvido.
type CompletedBoard = Matrix Int 

-- Cria um novo tabuleiro a partir das opções fornecidas.
-- O tabuleiro começa com todos os quadrados tendo todas as possibilidades.
-- Se o n for maior que a altura máxima, quer dizer que algumas casas podem ser
-- parques.
newBoard :: Options -> Board
newBoard (Options n maxHeight _ _) =
  Matrix.matrix n n (const $ IntSet.fromAscList [minHeight..maxHeight])
  where minHeight = if n > maxHeight then 0 else 1
 
-- Retorna a matrix atualizada na posição se o valor retornado for `Just`.
matrixUpdateMonad :: (a -> Maybe a) -> (Int, Int) -> Matrix a -> Maybe (Matrix a)
matrixUpdateMonad f (i, j) mat = do
  x <- f (Matrix.getElem i j mat)
  return (Matrix.setElem x (i, j) mat)


-- Verifica se uma linha satisfaz as condições de visão.
lineSatisfiesViews :: (Int, Int) -> Vector Int -> Bool
lineSatisfiesViews (startReq, endReq) line = 
  let
    fold e (count, maxH) = if e > maxH then (count + 1, e) else (count, maxH)
    fromStart = Vector.foldl (flip fold) (0, 0) line & fst & (== startReq)
    fromEnd = Vector.foldr fold (0, 0) line & fst & (== endReq)
  in
  (startReq == 0 || fromStart) && (endReq == 0 || fromEnd)


-- Verifica se o tabuleiro satifaz todos os pontos de vistas.
boardSatisfiesViews :: Options -> FilledBoard -> Bool
boardSatisfiesViews opts board = colsValid && rowsValid
  where
    n = _n opts
    colsValid = flip List.all [1..n] (\j ->
      Matrix.getCol j board
      & lineSatisfiesViews (colViewReq opts j)) 

    rowsValid = flip List.all [1..n] (\j ->
      Matrix.getRow j board
      & lineSatisfiesViews (rowViewReq opts j)) 


-- Tenta transformar um tabuleiro em preenchido.
tryToFill :: Board -> Maybe FilledBoard
tryToFill board
  -- Verifica se cada um dos quadrados tem apenas um estado possível.
  | any (\s -> (IntSet.size s) /= 1) (Matrix.toList board) = Nothing
  | otherwise = Just $ Matrix.matrix n n (\(i, j) -> IntSet.findMin $ Matrix.getElem i j board)
  where n = Matrix.ncols board


-- Tenta transformar um tabuleiro preenchido em completo.
tryToComplete :: Options -> FilledBoard -> Maybe CompletedBoard
tryToComplete opts filled 
  | boardSatisfiesViews opts filled = Just filled
  | otherwise = Nothing



{-------- Tratamento de possibilidades --------}

-- Retorna todas as casas alcançáveis por uma dada posição (Menos a propria posição).
positions :: Options -> (Int, Int) -> [(Int, Int)] 
positions opts (i, j) =
  let
    (Options n _ _ checkDiags) = opts 
    primaryDiag = if checkDiags && (i == j)
      then [(k, k) | k <- [1..n], (k, k) /= (i, j)]
      else []
    secondaryDiag = if checkDiags && (i == n - j + 1)
      then [(k, n - k + 1) | k <- [1 .. n], (k, n - k + 1) /= (i, j)]            
      else []
  in
    primaryDiag ++
    secondaryDiag ++
    [(i, k) | k <- [1..n], k /= j] ++ -- Linha
    [(k, j) | k <- [1..n], k /= i]    -- Coluna


-- Representa uma ação de atualização das possibilidades:
-- * Collapse: força a casa a assumir tal valor.
-- * Prune: remove o valor das possibilidades da casa.
data ClueUpdate = Collapse Int (Int, Int) | Prune Int (Int, Int)
  deriving Show


-- Utiliza uma ação de atualização para modificar o tabuleiro.
updateClues :: Options -> [ClueUpdate] -> Board -> Maybe Board
updateClues opts clues initialBoard = aux clues initialBoard
  where
    (Options n maxHeight _ _) = opts
    collapseTile x tile
      | not $ x `IntSet.member` tile = Nothing
      | otherwise = Just (IntSet.singleton x)

    pruneTile x tile
      -- Remover a única possibilidade de uma casa gera uma contradição.
      | x `IntSet.member` tile && IntSet.size tile == 1 = Nothing
      -- A não ser que a unica possibilidade seja 0.
      | 0 `IntSet.member` tile && IntSet.size tile == 1 = Just tile
      | otherwise = Just (IntSet.delete x tile)
    
    canCollapseZero :: (Int, Int) -> Board -> Bool
    canCollapseZero pos board = 
      positions opts pos
        & List.map (\(i, j) -> Matrix.getElem i j board)
        & List.filter (\poss -> poss == IntSet.singleton 0)
        & List.length
        & (== n - maxHeight - 1)
  
    aux :: [ClueUpdate] -> Board -> Maybe Board
    aux [] board = Just board

    aux ((Collapse x pos) : rest) board = do
      -- Ao colapsar uma casa, deve-se remover a possibilidade desse valor das outras.
      let rest' = if x /= 0 || canCollapseZero pos board
          then List.map (Prune x) (positions opts pos) ++ rest
          else [] ++ rest

      nextBoard <- matrixUpdateMonad (collapseTile x) pos board
      aux rest' nextBoard

    -- Ao remover uma possibilidade, deve-se checar se a casa não acabou sendo colapsada
    aux ((Prune x (i, j)) : rest) board = do
      let tile = Matrix.getElem i j board
      -- Se ela foi collapsada, troca a instrução que deve ser executada.
      if x `IntSet.member` tile && IntSet.size tile == 2 then do
        let newX = IntSet.delete x tile & IntSet.findMin
        aux ((Collapse newX (i, j)) : rest) board

      else do
        nextBoard <- matrixUpdateMonad (pruneTile x) (i, j) board
        aux rest nextBoard


-- Colapsa as possibilidades de valores para `x` em uma determinada posição,
-- se o colapso deixar o tabuleiro inválido, retorna `Nothing`.
-- Ao colapsar para um zero, ele só retira das outras possibilidades se já tiverem
-- ocorrido todos os zeros possíveis (== n - maxHeight).
collapse :: Options -> Int -> (Int, Int) -> Board -> Maybe Board
collapse opts x pos board = updateClues opts [Collapse x pos] board
 

-- Retorna a primeira posição com menor número de possibilidades
-- (> 2 pois se tem apenas uma opção, já está colapsada).
optimalCollapse :: Board -> (Int, Int)
optimalCollapse board =
  Matrix.toList board
    & List.zip [(i, j) | i <- [1..n], j <- [1..n]]
    & List.filter (\(_, poss) -> (IntSet.size poss) > 1)
    & List.minimumBy (\(_, p1) (_, p2) -> compare (IntSet.size p1) (IntSet.size p2))
    & fst
  where
    n = Matrix.ncols board


-- Usa as visibilidades para remover opções inválidas.
clueOfN :: Options -> Int -> Int -> [[Int]]
clueOfN opts start end = 
  [1..n]
    & List.map (\i -> min (hCeil start i) (hCeil end (n - i + 1)))
    & List.map (\maxReq -> if maxReq <= maxHeight then [maxReq..maxHeight] else [])
  where
    n = _n opts
    maxHeight = _maxHeight opts
    hCeil req pos = pos + 1 + maxHeight - req



{------------ Resolução ------------}

-- Usa as restrições de visão para remover possibilidades inválidas.
-- Caso as opções de visão deixem o puzzle não-resolvível, retorna `Nothing`.
initialPrune :: Options -> Board -> Maybe Board
initialPrune opts = updateClues opts (rowClues ++ colClues)
  where
    transposeClue :: ClueUpdate -> ClueUpdate
    transposeClue (Collapse x (i, j)) = Collapse x (j, i)
    transposeClue (Prune x (i, j)) = Prune x (j, i)
  
    lineClues :: Vector (Int, Int) -> [ClueUpdate]
    lineClues line = line
      & Vector.toList
      & List.zip [1..]
      & List.concatMap (\(i, (start, end)) ->
          clueOfN opts start end
            & List.zip [1..]
            & List.concatMap (\(j, cs) ->
              cs & List.map (\x -> Prune x (i, j)))) 

    rowClues = lineClues (_rows $ _viewReq opts) 
    colClues = lineClues (_cols $ _viewReq opts)
      & List.map transposeClue 


-- É usado uma função auxiliar para não ter que repassar as opções do jogo,
-- já que elas não mudarão entre iterações.
solve :: Options -> Board -> Maybe CompletedBoard
solve opts initialBoard = initialPrune opts initialBoard >>= step
  where
    step :: Board -> Maybe CompletedBoard
    step board = 
      -- Checa se o puzzle foi preenchido.
      case tryToFill board of
        -- Se tiver sido preenchido tem que checar se está válido.
        Just filled -> tryToComplete opts filled
        Nothing -> do
          -- Achar a posição com menos opções.
          let (i, j) = optimalCollapse board
          let possibilities = IntSet.toList $ Matrix.getElem i j board
          let newBoards = List.map (\x -> collapse opts x (i, j) board) possibilities

          -- Tentar resolver todas as possibilidades retornando a primeira que é válida
          asum $ List.map (>>= step) newBoards



{-------- Teste --------}
       
main :: IO ()
main = do
  -- Exemplo dado na página do puzzle.
  let opts = Options 6 5 ( ViewReq (Vector.fromList [ (4, 2), (3, 2), (3, 2), (3, 1), (1, 2), (2, 2) ]) (Vector.fromList [ (3, 2), (3, 1), (1, 4), (2, 3), (1, 5), (3, 3) ]) ) False
  let board = newBoard opts
  print $ solve opts board
