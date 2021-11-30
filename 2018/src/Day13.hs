{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day13 where

import Control.Applicative (many, empty)
import Control.Arrow ((&&&))
import Control.Lens
    ( Iso', (^.), (%~), (.~), (?~), (&), _Left, _Right, _2, preview, over
    , view, at, iso, ix
    )
import Control.Monad ((>=>), foldM)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Generics.Labels ()  -- for #lens orphan instance
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>), (<.))
import GHC.Generics (Generic)
import Linear (V2(V2), _x, _y, _yx)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


data Track
    = Horizontal
    | Vertical
    | NorthEastCurve
    | SouthEastCurve
    | Intersection
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Direction
    = North
    | East
    | South
    | West
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Choice
    = GoLeft
    | GoStraight
    | GoRight
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Cart = MkCart
    { direction :: Direction
    , choice :: Choice
    } deriving (Eq, Generic, Ord, Read, Show)

data Railway = MkRailway
    { railTracks :: Map (V2 Int) Track
    , railCarts :: Map (V2 Int) Cart
    } deriving (Eq, Generic, Ord, Read, Show)

data SortOfCrash
    = Collision
    | NoRails
    | BadRails
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Crash = MkCrash
    { position :: V2 Int
    , crashSort :: SortOfCrash
    , curState :: Railway
    } deriving (Eq, Ord, Read, Show)

parseTrack :: Parser Track
parseTrack = asum
    [ Par.Ch.char '-'  $> Horizontal
    , Par.Ch.char '|'  $> Vertical
    , Par.Ch.char '/'  $> NorthEastCurve
    , Par.Ch.char '\\' $> SouthEastCurve
    , Par.Ch.char '+'  $> Intersection
    ]

prettyTrack :: Track -> Char
prettyTrack = \case
    Horizontal     -> '-'
    Vertical       -> '|'
    NorthEastCurve -> '/'
    SouthEastCurve -> '\\'
    Intersection   -> '+'

parseCart :: Parser Cart
parseCart = asum
    [ Par.Ch.char '^' $> makeCart North
    , Par.Ch.char '>' $> makeCart East
    , Par.Ch.char 'v' $> makeCart South
    , Par.Ch.char '<' $> makeCart West
    ]
  where
    makeCart dir = MkCart
        { direction = dir
        , choice = GoLeft
        }

prettyCart :: Cart -> Char
prettyCart = direction .> \case
    North -> '^'
    East  -> '>'
    South -> 'v'
    West  -> '<'

parseTile :: Parser (Maybe (Either Track Cart))
parseTile = asum
    [ (Just <. Left)  <$> parseTrack
    , (Just <. Right) <$> parseCart
    , Nothing <$ Par.Ch.char ' '
    ]

cartToTrack :: Cart -> Track
cartToTrack = direction .> \case
    North -> Vertical
    East  -> Horizontal
    South -> Vertical
    West  -> Horizontal

parseRailway :: Parser Railway
parseRailway = do
    grid <- zip [0..] <$> Par.sepEndBy parseRow Par.Ch.newline
    let objects = do
            (y, row)  <- grid
            (x, tile) <- row
            case tile of
                Nothing  -> empty
                Just obj -> pure (V2 x y, obj)
    let tracks = mapMaybe (over _2 (preview _Left) .> sequence) objects
    let carts = mapMaybe (over _2 (preview _Right) .> sequence) objects
    pure $ MkRailway
        { railTracks =
            let underTracks = fmap (_2 %~ cartToTrack) carts
            in  Map.fromList (tracks <> underTracks)
        , railCarts = Map.fromList carts
        }
  where
    parseRow = zip [0..] <$> many parseTile

prettyRailway :: Railway -> Text
prettyRailway MkRailway{..} = Text.intercalate "\n" $ do
    y <- [infimum 0 ys .. supremum 0 ys]
    pure $ Text.pack $ do
        x <- [infimum 0 xs .. supremum 0 xs]
        let vec = V2 x y
        pure $ if
            | Just cart <- railCarts Map.!? vec   -> prettyCart cart
            | Just track <- railTracks Map.!? vec -> prettyTrack track
            | otherwise                           -> ' '
  where
    points = Map.keys railTracks <> Map.keys railCarts
    xs = view _x <$> points
    ys = view _y <$> points

asLines :: Iso' Text [Text]
asLines = iso Text.lines (Text.intercalate "\n")

prettyCrash :: Crash -> Text
prettyCrash MkCrash{..} = prettyRailway curState
    & asLines . ix (position ^. _y) . ix (position ^. _x) .~ 'X'

getInput :: Text -> Either String Railway
getInput = runParser (parseRailway <* Par.eof) "day-13"

unitVecInDir :: Direction -> V2 Int
unitVecInDir = \case
    North -> V2 0 (-1)
    East  -> V2 1 0
    South -> V2 0 1
    West  -> V2 (-1) 0

nextChoice :: Choice -> Choice
nextChoice = \case
    GoLeft     -> GoStraight
    GoStraight -> GoRight
    GoRight    -> GoLeft

turnRight :: Direction -> Direction
turnRight = \case
    North -> East
    East  -> South
    South -> West
    West  -> North

turnLeft :: Direction -> Direction
turnLeft = \case
    North -> West
    East  -> North
    South -> East
    West  -> South

choose :: Choice -> Direction -> Direction
choose = \case
    GoLeft     -> turnLeft
    GoStraight -> id
    GoRight    -> turnRight

intersect :: Cart -> Cart
intersect cart = cart
    & #direction %~ choose (choice cart)
    & #choice %~ nextChoice

flipNorthEast :: Direction -> Direction
flipNorthEast = \case
    North -> East
    East  -> North
    South -> West
    West  -> South

flipSouthEast :: Direction -> Direction
flipSouthEast = \case
    North -> West
    East  -> South
    South -> East
    West  -> North

isVertical :: Direction -> Bool
isVertical = flip elem [North, South]

isHorizontal :: Direction -> Bool
isHorizontal = flip elem [East, West]

moveOn :: Track -> Cart -> Maybe Cart
moveOn track cart@MkCart{..} = case track of
    Intersection   -> Just (intersect cart)
    NorthEastCurve -> Just (cart & #direction %~ flipNorthEast)
    SouthEastCurve -> Just (cart & #direction %~ flipSouthEast)
    Vertical       -> if isVertical direction then Just cart else Nothing
    Horizontal     -> if isHorizontal direction then Just cart else Nothing

tickCart :: V2 Int -> Cart -> Railway -> Either Crash Railway
tickCart pos cart railway@MkRailway{..} =
    if nextPos `Map.member` railCarts
        then Left $ makeCrash Collision
        else case nextCart of
            Left sort  -> Left $ makeCrash sort
            Right next ->
                let updated = railway
                        & #railCarts . at pos .~ Nothing
                        & #railCarts . at nextPos ?~ next
                in  Right updated
  where
    nextPos = pos + unitVecInDir (direction cart)
    makeCrash sort = MkCrash
        { position = nextPos
        , crashSort = sort
        , curState = railway
            & #railCarts . at pos .~ Nothing
            & #railCarts . at nextPos .~ Nothing
        }
    nextCart = do
        track <- maybeToEither NoRails $ railTracks Map.!? nextPos
        maybeToEither BadRails $ moveOn track cart

tick :: Railway -> Either Crash Railway
tick railway@MkRailway{..} = foldM (flip $ uncurry tickCart) railway carts
  where
    carts = List.sortOn (fst .> view _yx) $ Map.toList railCarts

ticks :: Int -> Railway -> Either Crash Railway
ticks n
    | n <= 0    = pure
    | otherwise = tick >=> ticks (n - 1)

tickUntilCrash :: Railway -> Crash
tickUntilCrash = tick .> either id tickUntilCrash

part1 :: Railway -> (Int, Int)
part1 = tickUntilCrash .> position .> (view _x &&& view _y)

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-13.txt"
    case getInput text of
        Left err      -> die err
        Right railway -> do
            print $ part1 railway
