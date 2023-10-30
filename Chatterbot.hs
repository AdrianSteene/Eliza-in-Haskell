module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe
import Debug.Trace
 

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain 
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase) 
stateOfMind bb = do
  gen <- newStdGen 
  let randomNumber = fst (random gen :: (Double, StdGen))
  return ( \inp -> rulesApply (map (\(a, b) -> (a, (pick (randomNumber) b))) bb) inp)  

--------------------------------------------------------

rulesApply :: [PhrasePair] -> Phrase -> (Phrase)
rulesApply pair p 
  | transform == Nothing = words ""
  | otherwise = (unmaybe)
  where 
    transform = transformationsApply "*" (reflect) pair p
    unmaybe = fromJust transform
 
reflect :: Phrase -> Phrase
reflect [] = []
reflect (p:ps) 
  | exists == Nothing = p : reflect ps
  | otherwise = unmaybe : reflect ps
  where exists = lookup p reflections
        unmaybe = fromJust exists  
 
reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]

---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|?") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile tb = map ( \(a, b) -> (words (map toLower a), (map words (map ( \(z) -> map toLower z ) b))) ) tb
--rulesCompile tb = map ( \(a, b) -> (prepare a, (map prepare b)) ) tb
--------------------------------------

reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply p x = case ( transformationsApply "*" (reflect) p x ) of
  Nothing -> x
  Just xs -> reductionsApply p xs

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute x (h:hs) ys  | x == h = ys ++ tl
                 | otherwise = h : tl
    where tl = substitute x hs ys

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc (p:ps) (l:ls) |  
  wc == p = orElse (singleWildcardMatch (p:ps) (l:ls)) (longerWildcardMatch (p:ps) (l:ls))
  | p == l = match wc ps ls
  | otherwise = Nothing

-- Help method to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) =  mmap (const[x]) (match wc ps xs)
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------


transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f (xs) (ys,zs) = mmap (substitute wc zs . f) (match wc ys xs)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (a:as) xs
  | test == Nothing = transformationsApply wc f as xs
  | otherwise = test
  where test = transformationApply wc f xs (a) 


