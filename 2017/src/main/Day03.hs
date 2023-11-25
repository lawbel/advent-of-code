module Day03 where

import Control.Lens ((+~), (-~))
import Flow ((.>))
import Linear.V2 (V2(V2), _x, _y)


data Direction = North | East | South | West
    deriving (Bounded, Enum, Eq, Ord, Show)

isVertical :: Direction -> Bool
isVertical = \case
    North -> True
    South -> True
    _ -> False

rotate :: Direction -> Direction
rotate = fromEnum .> subtract 1 .> (`mod` 4) .> toEnum

move :: Int -> Direction -> V2 Int -> V2 Int
move n = \case
    North -> _y +~ n
    East  -> _x +~ n
    South -> _y -~ n
    West  -> _x -~ n

spiral :: [V2 Int]
spiral = iter (V2 0 0) 0 0 South
  where
    iter :: V2 Int -> Int -> Int -> Direction -> [V2 Int]
    iter base stride step dir
        | step < stride = move step dir base : iter base stride (step + 1) dir
        | otherwise =
            let next = if isVertical dir then stride + 1 else stride
            in  iter (move stride dir base) next 0 (rotate dir)

manhattan :: V2 Int -> Int
manhattan (V2 x y) = abs x + abs y

main :: IO ()
main = do
    print $ manhattan $ spiral !! pred 347991
