module Main where

import           Control.Monad.Eff
import           Control.Monad.Eff.Console
import           Control.Monad.ST
import           Control.MonadPlus
import           DOM.Timer
import           Data.Int (toNumber)
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           Data.Traversable
import           Prelude

import           Graphics.Canvas (getContext2D, getCanvasElementById, Canvas())
import           Graphics.Canvas.Free

data IsAlive = Alive | Dead

newtype World = World (S.Set Cell)

instance showWorld :: Show World where
  show (World w) = show w

newtype Cell = Cell {x :: Int, y :: Int}

instance showCell :: Show Cell where
  show (Cell c) = "[" <> show c.x <> "|" <> show c.y <> "]"

instance ordCell :: Ord Cell where
  compare (Cell c1) (Cell c2)
    | c1.x == c2.x = c1.y `compare` c2.y
    | otherwise = c1.x `compare` c2.x

instance eqCell :: Eq Cell where
  eq (Cell c1) (Cell c2) = eq c1.x c2.x && eq c1.y c2.y

neighbours :: Cell -> List Cell
neighbours (Cell c) = do
  x <- -1..1
  y <- -1..1
  guard (x /= 0 || y /= 0)
  return $ Cell {x: (c.x + x), y: (c.y + y)}

candidates :: World -> List Cell
candidates (World w) =
  concatMap neighbours $ S.toList w

shouldLive :: IsAlive -> Int -> Boolean
shouldLive Alive n = n == 2 || n == 3
shouldLive Dead n = n == 3

isAlive :: World -> Cell -> IsAlive
isAlive (World w) c =
  if S.member c w
  then Alive
  else Dead

evolve :: World -> World
evolve w =
  candidates >>>
  sort >>>
  group >>>
  filter shouldLive' >>>
  toWorld $ w
 where
    isAlive' = isAlive w
    shouldLive' l@(Cons h _) = shouldLive (isAlive' h) (length l)
    toCell (Cons h _) = h
    toWorld = map toCell >>> S.fromList >>> World

cells1 = (Cons (Cell {x: 1, y:1})
                         (Cons (Cell {x:1, y:2})
                          (Cons (Cell {x:1, y:3})
                           Nil)))

w3 = (World (S.fromList cells1))

render :: forall eff. World -> Eff (canvas :: Canvas | eff) Unit
render (World w) = do
  Just x <- getCanvasElementById "canvas"
  ctx <- getContext2D x
  runGraphics ctx do
    clearWorld
    traverse livingCell cells
  return unit
  where
    cells = S.toList w

clearWorld :: Graphics Unit
clearWorld = clearRect {x: -300.0, y: -300.0, w: 600.0, h: 600.0}

livingCell :: Cell -> Graphics Unit
livingCell (Cell c)  = do
  setFillStyle "#FF0000"
  fillRect {x: (toNumber c.x) * 10.0, y: (toNumber c.y) * 10.0, w: 10.0, h: 10.0}

initialWorld = w3

main = do
  state <- newSTRef initialWorld
  interval 1000 (modifySTRef state evolve >>= render)
