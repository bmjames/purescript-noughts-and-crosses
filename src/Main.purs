module Main where

import Prelude

import Control.Alt                      ((<|>), alt)
import Control.Bind                     ((<=<))
import Control.Monad.Eff                (Eff())
import Control.Monad.ST
import Control.Plus                     (class Plus, empty)

import Data.Array                       (concat, nub, replicate)
import Data.Array.ST
import Data.Array.WordsLines            (unlines, unwords)
import Data.Foldable                    (class Foldable, any, foldl)
import Data.Lens                        (LensP, TraversalP, (.~), lens, view)
import Data.Lens.Index                  (ix)
import Data.Matrix                      (Mat, columns, fromArray, transpose)
import Data.Maybe                       (Maybe(..), isNothing, maybe)
import Data.Traversable                 (sequence)
import Data.TypeNat                     (Three)

import Halogen
import Halogen.Util                     (awaitBody, runHalogenAff)
import Halogen.HTML.Indexed             as H
import Halogen.HTML.Events.Indexed      as E

data Query a = Move Int Int a

data Player = X | O

instance eqPlayer :: Eq Player where
  eq X X = true
  eq O O = true
  eq _ _ = false

instance showPlayer :: Show Player where
  show X = "X"
  show O = "O"

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

newtype Board = Board (Array (Array (Maybe Player)))

instance showBoard :: Show Board where
  show (Board rows) = unlines (map showRow rows)
    where
    showRow = unwords <<< map (maybe " " show)

boardRows :: LensP Board (Array (Array (Maybe Player)))
boardRows = lens (\(Board rows) -> rows) (\_ -> Board)

emptyBoard :: Board
emptyBoard = Board $ replicate 3 (replicate 3  Nothing)

updateBoard :: Int -> Int -> Player -> Board -> Board
updateBoard x y player = (boardRows <<< atIndex y <<< atIndex x) .~ Just player
  where
    atIndex :: forall a. Int -> TraversalP (Array a) a
    atIndex i = ix i

transposeBoard :: Board -> Board
transposeBoard (Board rows) =
  let
    matrix :: Mat Three (Maybe Player)
    matrix = fromArray (concat rows)
  in
    Board $ columns (transpose matrix)

type State = { turn :: Player, board :: Board }

initialState :: State
initialState = { turn: O, board: emptyBoard }

data EndGame = Stalemate | Winner Player

instance showEndGame :: Show EndGame where
  show Stalemate  = "Stalemate!"
  show (Winner p) = show p ++ " has won!"

endGame :: Board -> Maybe EndGame
endGame board = map Winner (choice (map lineWinner lines)) <|> stalemate
  where
  rows = view boardRows board

  stalemate = if any (any isNothing) rows then Nothing else Just Stalemate

  lines :: Array (Array (Maybe Player))
  lines = concat [ rows, view boardRows (transposeBoard board), diagonals board ]

  lineWinner :: Array (Maybe Player) -> Maybe Player
  lineWinner row = do
    players <- sequence row
    case nub players of [p] -> Just p
                        _   -> Nothing

  diagonals :: Board -> Array (Array (Maybe Player))
  diagonals (Board [ [x, _, a]
                   , [_, y, _]
                   , [b, _, z] ]) = [ [x, y, z], [b, y, a] ]

ui :: forall g. Component State Query g
ui = component { render, eval }

render :: State -> ComponentHTML Query
render state =
  H.div_
    [ H.p_ [ H.text info ], renderBoard ]
  where
    ended = endGame state.board

    info = maybe (show state.turn ++ "'s turn to move") show ended

    renderBoard = H.table_ $ map renderRow $ zipWithIndex (view boardRows state.board)

    renderRow { value = row, index = y } = H.tr_ (map renderCell (zipWithIndex row))
      where
      renderCell { value = Just player        } = H.td_ [ H.text (show player) ]
      renderCell { value = Nothing, index = x } = H.td_ $ maybe [ button x y ] (\_ -> []) ended

    button x y = H.button
      [ E.onClick (E.input_ (Move x y)) ]
      [ H.text "?" ]

eval :: forall g. Natural Query (ComponentDSL State Query g)
eval (Move x y next) = do
  modify (\state -> { turn: nextPlayer state.turn, board: updateBoard x y state.turn state.board })
  pure next

choice :: forall f m a. (Plus m, Foldable f) => f (m a) -> m a
choice = foldl alt empty

zipWithIndex :: forall a. Array a -> Array (Assoc a)
zipWithIndex xs = pureST (zipWithIndexST xs)

zipWithIndexST :: forall a h r. Array a -> Eff (st :: ST h | r) (Array (Assoc a))
zipWithIndexST = toAssocArray <=< thaw

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui initialState body
