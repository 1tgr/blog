$$region(1)
 # Python region looks like this:
import re, collections

> -- Haskell region looks like this:
> module Main where
>
> import Char
> import qualified Data.List as List
> import qualified Data.Map as Map
> import qualified Data.Set as Set
> import Data.Ord
> import IO
> import List
$$/region

$$region(2)
def words(text): return re.findall('[a-z]+', text.lower())

> lowerWords = filter (not . null) . map (map toLower . filter isAlpha) . words
$$/region

$$region(3)
def train(features):
    model = collections.defaultdict(lambda: 1)
    for f in features:
        model[f] += 1
    return model

> train = List.foldl' (\dict word -> Map.insertWith' (+) word (1::Int) dict) Map.empty 
$$/region

$$region(4)
NWORDS = train(words(file('big.txt').read()))

> readNWORDS = readFile "big.txt" >>= return . train . lowerWords
$$/region

$$region(5)
alphabet = 'abcdefghijklmnopqrstuvwxyz'

> alphabet = [ 'a' .. 'z' ]
$$/region

$$region(6)
def edits1(word):
   s = [(word[:i], word[i:]) for i in range(len(word) + 1)]
   deletes    = [a + b[1:] for a, b in s if b]
   transposes = [a + b[1] + b[0] + b[2:] for a, b in s if len(b)>1]
   replaces   = [a + c + b[1:] for a, b in s for c in alphabet if b]
   inserts    = [a + c + b     for a, b in s for c in alphabet]
   return set(deletes + transposes + replaces + inserts)

> edits1 word =
>     let s = [ (take i word, drop i word) | i <- [ 0 .. length word ] ]
>         deletes    = [ a ++ tail b | (a, b) <- s, not $ null b ]
>         transposes = [ a ++ b!!1 : b!!0 : drop 2 b | (a, b) <- s, not $ null b, not $ null $ tail b ]
>         replaces   = [ a ++ c : tail b | (a, b) <- s, c <- alphabet, not $ null b ]
>         inserts    = [ a ++ c : b | (a, b) <- s, c <- alphabet ]
>     in Set.fromList (deletes ++ transposes ++ replaces ++ inserts)
$$/region

$$region(7)
def known_edits2(word):
    return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)

> known_edits2 knownWords = Set.unions . Set.elems . Set.map (Set.intersection knownWords . edits1) . edits1
$$/region

$$region(8)
def known(words): return set(w for w in words if w in NWORDS)

> -- no Haskell equivalent
$$/region

$$region(9)
def correct(word):
    candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
    return max(candidates, key=NWORDS.get)

> correct nwords word = 
>     let knownWords = Map.keysSet nwords
>         candidates = Set.elems
>                    $ head
>                    $ filter (not . Set.null)
>                    $ [ Set.intersection knownWords $ Set.singleton word,
>                        Set.intersection knownWords $ edits1 word,
>                        known_edits2 knownWords word,
>                        Set.singleton word ]
>       in maximumBy (comparing (\w -> w `Map.lookup` nwords)) candidates
$$/region

$$region(10)
> prompt nwords = do
>     eof <- isEOF
>     if eof
>       then return ()
>       else do
>           getLine >>= putStrLn . correct nwords
>           prompt nwords
> 
> main = readNWORDS >>= prompt
$$/region

