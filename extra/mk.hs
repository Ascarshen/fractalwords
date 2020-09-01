import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.STRef

import Data.Array.Unboxed as A
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unsafe (unsafeFreeze)

import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy

import Data.Char as C hiding (isAlpha, toUpper)
import Data.List as L
import Data.Map as M
import Data.Ord as O
import Data.Set as S
import Data.Word as W

import System.Environment (getArgs)

equating f a b = f a == f b

chain :: Ord a => Set a -> [a] -> UArray (Int, Int) Word64
chain wordSet input@(hinput:tinput) = runST $ do
  let n = S.size wordSet
      bs = ((0,0),((n-1),(n-1)))
      ix w = S.size (fst (w `S.split` wordSet))
      inc x | x == maxBound = maxBound
            | otherwise = x + 1
  arr <- newArray bs 0 :: ST s (STUArray s (Int, Int) Word64)
  context <- newSTRef (ix hinput)
  forM_ (tinput ++ [hinput]) $ \word -> do
    i <- readSTRef context
    let j = ix word
    writeArray arr (i, j) . inc =<< readArray arr (i, j)
    writeSTRef context j
  unsafeFreeze arr

prettyChain :: Int -> Set Strict.ByteString -> UArray (Int, Int) Word64 -> Lazy.ByteString
prettyChain maxLinks wordSet chain = Lazy.fromChunks . L.concat $
  [ L.intersperse spc ([context, col] ++ (L.map fst . L.take maxLinks . L.sortBy (flip (comparing snd))) [ (word, count) | (word, j) <- S.toList wordSet `L.zip` [i+0-i..], let count = chain A.! (i, j), count > 0 ]) ++ [eol] | (context, i) <- S.toList wordSet `L.zip` [0..] ]
  where
    eol = Strict.pack [fromIntegral $ ord '\n']
    spc = Strict.pack [fromIntegral $ ord ' ']
    col = Strict.pack [fromIntegral $ ord ':']

histogram :: Ord a => [a] -> Map a Int
histogram = L.foldl' inc M.empty
  where
    inc m w = case M.lookup w m of
      Nothing -> M.insert w 1 m
      Just k -> let k' = k + 1 in k' `seq` M.insert w k' m

commonest :: Ord a => Map a Int -> [a]
commonest = L.map fst . L.sortBy (flip (comparing snd)) . M.toList

toWords :: String -> Lazy.ByteString -> [Strict.ByteString]
toWords tag = L.concatMap (lineWords tag) . L.map toStrict . llines

lineWords :: String -> Strict.ByteString -> [Strict.ByteString]
lineWords "irc" = \l -> case Strict.uncons (Strict.drop 9 l) of
  Just (c, l)
    | c == fromIntegral (ord '-') -> []
    | otherwise -> (L.drop 1 . L.filter isWord . Strict.groupBy (equating isAlpha) . Strict.map toUpper) l
  Nothing -> []
lineWords "mail" =  L.drop 1 . L.filter isWord . Strict.groupBy (equating isAlpha) . Strict.map toUpper

isWord :: Strict.ByteString -> Bool
isWord s = 4 <= Strict.length s && Strict.all isAlpha s

isAlpha :: Word8 -> Bool
isAlpha = \c -> a <= c && c <= z
  where
    a = fromIntegral (ord 'A')
    z = fromIntegral (ord 'Z')

toUpper :: Word8 -> Word8
toUpper = \c -> if a <= c && c <= z then c - a + bigA else c
  where
    a = fromIntegral (ord 'a')
    z = fromIntegral (ord 'z')
    bigA = fromIntegral (ord 'A')

toStrict :: Lazy.ByteString -> Strict.ByteString
toStrict = Strict.concat . Lazy.toChunks

llines :: Lazy.ByteString -> [Lazy.ByteString]
llines s = if Lazy.null s then [] else l : llines (Lazy.drop 1 ls)
  where
    (l, ls) = Lazy.span neol s

neol = \c -> c /= eol
  where
    eol = fromIntegral (ord '\n')

slines :: Strict.ByteString -> [Strict.ByteString]
slines s = if Strict.null s then [] else l : slines (Strict.drop 1 ls)
  where
    (l, ls) = Strict.span neol s

nl = Strict.pack [fromIntegral $ ord '\n']

main :: IO ()
main = do
  args <- getArgs
  case args of
    [tag, "words", wordCount] -> Lazy.interact (Lazy.fromChunks . L.concatMap (\x->[x,nl]) . L.take (read wordCount) . commonest . histogram . toWords tag)
    [tag, "chain", wordFile] -> do
      wordList <- (S.fromList . slines) `fmap` Strict.readFile wordFile
      Lazy.interact (prettyChain 32 wordList . chain wordList . L.filter (`S.member` wordList) . toWords tag)
