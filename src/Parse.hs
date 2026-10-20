module Parse
  ( module Parse
  , Alternative(..)
  , optional
  ) where

{- HLINT ignore -}

import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import Codec.Binary.UTF8.Generic (toString)
import Data.Function (fix)

-- | Parser Data backend

data Input = Input
  { inputBytes :: !ByteString
  , inputPos   :: !Int
  } deriving (Show)

fromByteString :: ByteString -> Input
fromByteString bs = Input bs 0

fromString :: String -> Input
fromString = fromByteString . BS.pack

pos :: Input -> Int
pos = inputPos

-- Can you get the element `pos` ahead (exclusive bound)?
canGet :: Int -> Input -> Bool
canGet dist (Input bs p) = i >= 0 && i < BS.length bs
  where i = p + dist

-- Can you take a substring of length `dist` from the current position?
canGetSubstr :: Int -> Input -> Bool
canGetSubstr dist (Input bs p) = dist >= 0 && p + dist <= BS.length bs

-- Substring of length `len` starting at the current position
takeLen :: Int -> Input -> ByteString
takeLen len (Input bs p) = BS.take len (BS.drop p bs)

-- Safe lookup of character at position `dist`
get :: Int -> Input -> Maybe Char
get dist inp@(Input bs p)
  | canGet dist inp = Just (BS.index bs (p + dist))
  | otherwise       = Nothing

-- Returns char at position `dist` if possible, on fail returns error string 
getOrFail :: Int -> Input -> Either String Char
getOrFail dist inp =
  maybe (Left ("Input backend error: out of bounds in get: " ++ show (pos inp)))
        Right
        (get dist inp)

-- Returns a new Input
next :: Int -> Input -> Input
next dist inp = inp { inputPos = inputPos inp + dist }



data ResultState a = Done Input a | Fail [String]
  deriving Show

type Success a r = Input -> a -> ResultState r
type Failure r   = Input -> [String] -> ResultState r

-- Continuation passing style parser
newtype Parser a = Parser
  { runParser :: forall r. Input -> Success a r -> Failure r -> ResultState r }

instance Functor Parser where
  fmap f p = Parser $ \inp ok err ->
    runParser p inp (\inp' a -> ok inp' (f a)) err

instance Applicative Parser where
  pure x    = Parser $ \inp ok _ -> ok inp x
  (<*>) pf px = pf >>= \f -> fmap f px

  (*>) p q = Parser $ \inp ok err ->
    runParser p inp (\inp' _ -> runParser q inp' ok err) err

  (<*) p q = Parser $ \inp ok err ->
    runParser p inp (\inp' a ->
      runParser q inp' (\inp'' _ -> ok inp'' a) err) err

instance Monad Parser where
  p >>= k = Parser $ \inp ok err ->
    runParser p inp (\inp' a -> runParser (k a) inp' ok err) err


instance Alternative Parser where
  empty   = Parser $ \inp _ err -> err inp ["empty"]

  (<|>) p q = Parser $ \inp ok err ->
    runParser p inp ok $ \_ msgs ->
      runParser q inp ok $ \inp' msgs' -> err inp' (msgs ++ msgs')


(>>|) :: Parser a1 -> (a1 -> a2) -> Parser a2
(>>|) p f = fmap f p

-- Run parser `p` on Input `inp`
parse :: Parser a -> Input -> ResultState a
parse p inp = runParser p inp Done (\_ errs -> Fail errs)

-- Run parser `p` on a string, Returns either the parsed object or an error message
parseStr :: Parser a -> String -> Either String a
parseStr p inp =
  case parse p (fromString inp) of
    Done _ s -> Right s
    Fail e -> Left (foldl (\acc s -> acc ++ "\n" ++ s) "" e)

-- Run parser `p` on a string, fails if the entire string is not consumed
parseAll :: Parser a -> String -> Maybe a
parseAll p s =
  case parse p (fromString s) of
    Done rest a | pos rest == length s -> Just a
    _                                  -> Nothing

-- Pure Failure
failure :: String -> Parser a
failure msg = Parser $ \inp _ok err ->
  err inp [msg]

-- Peek the next character, this always succeds
peek :: Parser (Maybe Char)
peek = Parser $ \inp ok _err ->
  ok inp (get 0 inp)

-- Parser that satisfies predicate `pr` on a character
char :: (Char -> Bool) -> Parser Char
char pr = Parser $ \inp ok err ->
  case get 0 inp of
    Just c | pr c -> ok (next 1 inp) c
    _             -> err inp ["satisfy failed at " ++ show (pos inp)]

-- Parser that advances `n` items in a string
advance :: Int -> Parser ()
advance n = Parser $ \inp ok err ->
  if canGetSubstr n inp
    then ok (next n inp) ()
    else err inp ["advance " ++ show n ++ " failed at " ++ show (pos inp)]

-- Parser that advances while `pr` is true (can not advance)
take_while :: (Char -> Bool) -> Parser String
take_while f = Parser $ \inp ok _err ->
  let dist = range inp 0 in
  let result = takeLen dist inp in
  ok (next dist inp) (toString result)
  where
    range inp l = 
      if (case get l inp of Just x -> (f x); _ -> False) then 
        range inp (l+1)
      else l

-- Parser that advances while `pr` is true (advances at least once)
take_while1 :: (Char -> Bool) -> Parser String
take_while1 f = Parser $ \inp ok err ->
  let dist = range inp 0 in
    if dist == 0 then err inp []
    else
  let result = takeLen dist inp in
  ok (next dist inp) (toString result)
  where
    range inp l = 
      if (case get l inp of Just x -> (f x); _ -> False) then 
        range inp (l+1)
      else l

-- Parser that advances while `pr` is true (can not advance)
(<?>) :: Parser a -> String -> Parser a
(<?>) p label = Parser $ \inp ok err ->
  runParser p inp ok (\inp' msg -> err inp' (label : msg))

-- Parser that matches specific String `str`
string :: String -> Parser String
string str = Parser $ \inp ok err ->
  let len = length str in
  if canGetSubstr len inp then
    let res = takeLen len inp in
     if (toString res) == str then ok (next len inp) (toString res)
     else err inp []
  else err inp []
  

lift :: Parser a1 -> (a1 -> a2) -> Parser a2
lift = ( >>| )

lift2 :: Applicative f => (a1 -> a2 -> b) -> f a1 -> f a2 -> f b
lift2 f m1 m2 = f <$> m1 <*> m2

lift3 :: Applicative f => (a1 -> a2 -> a3 -> b) -> f a1 -> f a2 -> f a3 -> f b
lift3 f m1 m2 m3 = f <$> m1 <*> m2 <*> m3


-- Runs parser `p` separated by separator parser `s` (returns at least one element)
sep_by1 :: (Alternative f, Monad f) => f a -> f a1 -> f [a1]
sep_by1 s p = fix (\m -> lift2 (:) p (s *> m <|> return []))

-- Runs parser `p` separated by separator parser `s` (can have no elements)
sep_by :: (Alternative f, Monad f) => f a -> f a1 -> f [a1]
sep_by s p = lift2 (:) p (s *> sep_by1 s p <|> return []) <|> return []
