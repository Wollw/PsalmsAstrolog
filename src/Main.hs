module Main where

import Prelude as P
import Graphics.Image as I

import Data.Char
import Data.List.Split
import System.Process
import Text.RegexPR

longest xss = snd $ P.maximum $ [(P.length xs, xs) | xs <- xss]

makePsalmImage n = do
    ns <- P.map (P.map ord) <$> lines <$> (readFile $ "Psalms/psalm"++n++".txt")
    return $ I.makeImageR VU (length ns, length $ longest ns)
                     (\(i,j)->PixelY
                       ((fromIntegral $ if j<length (ns!!i) then ns!!i!!j else 0)/128))

makePsalmImageColor n = do
    (Just (AstroPosition x _ y)) <- moon <$> getAstrolog
    ns <- P.map (P.map ord) <$> lines <$> (readFile $ "Psalms/psalm"++n++".txt")
    return $ I.makeImageR VU (length ns, length $ longest ns)
                     (\(i,j)->PixelRGB
                       ((fromIntegral $ if j<length (ns!!i) then ns!!i!!j else 0)/128)
                       ((fromIntegral $ if j<length (ns!!i) then i+x else 0)/256)
                       ((fromIntegral $ if j<length (ns!!i) then j+y else 0)/256))

writePsalmImage n = do
    i <- makePsalmImage $ show n++[] :: IO (Image VU Y Double)
    writeImage ("Psalms/psalm"++(show n)++".png") i

writePsalmImageColor n = do
    i <- makePsalmImageColor $ show n++[] :: IO (Image VU RGB Double)
    writeImage ("Psalms/psalm"++(show n)++".png") i


type Minute = Int
type Sign = String
type Second = Int

data AstroPosition = AstroPosition Minute Sign Second deriving Show

data Astrolog = Astrolog { sun :: Maybe AstroPosition
                         , moon :: Maybe AstroPosition
                         , mercury :: Maybe AstroPosition
                         , venus :: Maybe AstroPosition
                         , mars :: Maybe AstroPosition
                         , jupiter :: Maybe AstroPosition
                         , saturn :: Maybe AstroPosition
                         } deriving Show

main :: IO ()
main = putStrLn "Hello, Haskell!"

getAstrolog :: IO Astrolog
getAstrolog = do 
    ls <- lines <$> readProcess "astrolog" ["-n"] ""
    return $ Astrolog { sun     = f $ ls !! 3
                      , moon    = f $ ls !! 4
                      , mercury = f $ ls !! 5
                      , venus   = f $ ls !! 6
                      , mars    = f $ ls !! 7
                      , jupiter = f $ ls !! 8
                      , saturn  = f $ ls !! 9
                      }
  where
    f = toAstroPosition . head . words . (!!1) . splitOn ":"
    toAstroPosition s = case matchRegexPR "(\\d+)(\\w\\w\\w)(\\d+)" s of
      Just ((_,_),[(3,a),(2,b),(1,c)]) -> Just $ AstroPosition (read c) b (read a)
      _ -> Nothing
