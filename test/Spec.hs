module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.QuickCheck

import Parse (Parser, ResultState (..), fromString, parse, pos)
import URL

------------------------------------------------------------------------
-- Generators (yours, unchanged)
------------------------------------------------------------------------

listWithLength :: (Int, Int) -> Gen a -> Gen [a]
listWithLength (minLen, maxLen) gen = do
  len <- choose (minLen, maxLen)
  vectorOf len gen

genAlphaWithLength :: (Int, Int) -> Gen String
genAlphaWithLength lenRange =
  listWithLength lenRange (elements ['a'..'z'])

genSchemeStr :: Gen String
genSchemeStr = elements ["http", "https", "ftp", "ssh"]

genDomainNameStr :: Gen String
genDomainNameStr = do
  tld <- genAlphaWithLength (2, 4)
  sld <- genAlphaWithLength (2, 10)
  subdomains <- frequency [
      (3, return []),
      (1, listWithLength (1, 5) $ genAlphaWithLength (2, 10))
    ]
  let subdomainsString = foldr (\sub acc -> acc ++ sub ++ ".") "" subdomains
  return $ subdomainsString ++ sld ++ "." ++ tld

genPathStr :: Gen String
genPathStr = frequency [
    (3, return ""),
    (1, do
      segments <- listWithLength (1, 5) $ genAlphaWithLength (2, 10)
      return $ foldr (\seg acc -> "/" ++ seg ++ acc) "" segments
    )
  ]

genRequestStr :: Gen String
genRequestStr = frequency [
    (3, return ""),
    (1, do
      k <- genAlphaWithLength (2, 10)
      v <- genAlphaWithLength (2, 10)
      rest <- listWithLength (0, 4) $ do
        key <- genAlphaWithLength (2, 10)
        value <- genAlphaWithLength (2, 10)
        return (key, value)
      return $ "?" ++ k ++ "=" ++ v ++ foldr (\(key, value) acc -> "&" ++ key ++ "=" ++ value ++ acc) "" rest
    )
  ]

genFragmentStr :: Gen String
genFragmentStr =
  frequency [
    (3, return ""),
    (1, ("#" ++) <$> genAlphaWithLength (2, 10))
  ]

genUrlStr :: Gen String
genUrlStr = do
  schemeStr <- genSchemeStr
  domainStr <- genDomainNameStr
  pathStr <- genPathStr
  requestStr <- genRequestStr
  fragmentStr <- genFragmentStr
  return $ schemeStr ++ "://" ++ domainStr ++ pathStr ++ requestStr ++ fragmentStr

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- Succeeds only if the parser consumes the WHOLE input.
-- (parseStr accepts a prefix, which would hide bugs.)
parseAll :: Parser a -> String -> Maybe a
parseAll p s =
  case parse p (fromString s) of
    Done rest a | pos rest == length s -> Just a
    _                                  -> Nothing

splitOn :: Char -> String -> [String]
splitOn d s = case break (== d) s of
  (a, [])       -> [a]
  (a, _ : rest) -> a : splitOn d rest

------------------------------------------------------------------------
-- Reference implementations: what each piece SHOULD parse to.
-- They only need to handle the well-formed strings the generators make.
------------------------------------------------------------------------

expectedSchema :: String -> Schema
expectedSchema "http"  = HTTP
expectedSchema "https" = HTTPS
expectedSchema "ftp"   = FTP
expectedSchema other   = ETC other

-- "a.b.example.com" -> tdl "com", sdl "example", subDomain ["a","b"]
-- (subdomains kept in the order they are written)
expectedDomain :: String -> Domain
expectedDomain s =
  let labels = splitOn '.' s
      n      = length labels
  in Domain { tdl       = labels !! (n - 1)
            , sdl       = labels !! (n - 2)
            , subDomain = take (n - 2) labels
            }

-- "/a/b" -> ["a","b"], "" -> []
expectedPath :: String -> [String]
expectedPath "" = []
expectedPath s  = drop 1 (splitOn '/' s)

-- "?k=v&k2=v2" -> [Request "k" "v", Request "k2" "v2"], "" -> []
expectedRequests :: String -> [Request]
expectedRequests ""         = []
expectedRequests ('?' : qs) = map toReq (splitOn '&' qs)
  where toReq kv = case splitOn '=' kv of
          [k, v] -> Request { param = k, value = v }
          _      -> error ("bad generated request: " ++ kv)
expectedRequests s          = error ("bad generated query: " ++ s)

-- "#frag" -> Just "frag", "" -> Nothing
expectedFragment :: String -> Maybe String
expectedFragment ('#' : f) = Just f
expectedFragment _         = Nothing

------------------------------------------------------------------------
-- Properties: each component on its own
------------------------------------------------------------------------

prop_schema :: Property
prop_schema = forAll genSchemeStr $ \s ->
  counterexample ("input: " ++ show s) $
    parseAll parseSchema s === Just (expectedSchema s)

prop_domain :: Property
prop_domain = forAll genDomainNameStr $ \s ->
  counterexample ("input: " ++ show s) $
    parseAll parseDomain s === Just (expectedDomain s)

prop_path :: Property
prop_path = forAll genPathStr $ \s ->
  counterexample ("input: " ++ show s) $
    parseAll parsePath s === Just (expectedPath s)

prop_requests :: Property
prop_requests = forAll genRequestStr $ \s ->
  counterexample ("input: " ++ show s) $
    parseAll parseRequests s === Just (expectedRequests s)

prop_fragment :: Property
prop_fragment = forAll genFragmentStr $ \s ->
  counterexample ("input: " ++ show s) $
    parseAll parseFragment s === Just (expectedFragment s)

------------------------------------------------------------------------
-- Properties: whole URLs
------------------------------------------------------------------------

-- Every generated URL is accepted and fully consumed.
prop_urlAccepted :: Property
prop_urlAccepted = forAll genUrlStr $ \s ->
  counterexample ("input: " ++ show s) $
    case parseAll parseURL s of
      Just _  -> property True
      Nothing -> property False

-- Each part of the URL ends up in the right field.
prop_urlFields :: Property
prop_urlFields =
  forAll genSchemeStr     $ \sc ->
  forAll genDomainNameStr $ \d  ->
  forAll genPathStr       $ \p  ->
  forAll genRequestStr    $ \r  ->
  forAll genFragmentStr   $ \f  ->
    let s = sc ++ "://" ++ d ++ p ++ r ++ f
    in counterexample ("input: " ++ show s) $
         parseAll parseURL s === Just URL
           { schema   = expectedSchema sc
           , domain   = expectedDomain d
           , path     = expectedPath p
           , requests = expectedRequests r
           , fragment = expectedFragment f
           }

------------------------------------------------------------------------
-- Hand-written examples (edge cases the generators don't cover)
------------------------------------------------------------------------

prop_exampleFull :: Property
prop_exampleFull = once $
  parseAll parseURL "https://www.example.com/a/b?x=1&y=2#top"
    === Just URL
      { schema   = HTTPS
      , domain   = Domain { tdl = "com", sdl = "example", subDomain = ["www"] }
      , path     = ["a", "b"]
      , requests = [Request "x" "1", Request "y" "2"]
      , fragment = Just "top"
      }

-- "https" must not be read as "http" followed by a leftover "s".
prop_exampleHttpsNotHttp :: Property
prop_exampleHttpsNotHttp = once $
  fmap schema (parseAll parseURL "https://example.com") === Just HTTPS

prop_exampleMinimal :: Property
prop_exampleMinimal = once $
  parseAll parseURL "ftp://example.org"
    === Just URL
      { schema   = FTP
      , domain   = Domain { tdl = "org", sdl = "example", subDomain = [] }
      , path     = []
      , requests = []
      , fragment = Nothing
      }

-- Inputs that must be rejected.
prop_rejects :: Property
prop_rejects = conjoin
  [ counterexample ("should reject " ++ show s) (isNothing' (parseAll parseURL s))
  | s <- [ ""
         , "http//example.com"     -- missing ':'
         , "http:/example.com"     -- missing one '/'
         , "://example.com"        -- missing scheme
         , "http://com"            -- no second-level domain
         , "http://example."       -- empty TLD
         , "http://example.com#"   -- empty fragment
         , "http://example.com?x"  -- request without '='
         ]
  ]
  where isNothing' Nothing = True
        isNothing' _       = False

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

main :: IO ()
main = do
  results <- sequence
    [ check "schema"                prop_schema
    , check "domain"                prop_domain
    , check "path"                  prop_path
    , check "requests"              prop_requests
    , check "fragment"              prop_fragment
    , check "url accepted"          prop_urlAccepted
    , check "url fields"            prop_urlFields
    , check "example: full"         prop_exampleFull
    , check "example: https"        prop_exampleHttpsNotHttp
    , check "example: minimal"      prop_exampleMinimal
    , check "rejects bad input"     prop_rejects
    ]
  let failed = length (filter not results)
  unless (failed == 0) $ do
    putStrLn (show failed ++ " test group(s) failed")
    exitFailure

check :: Testable p => String -> p -> IO Bool
check name p = do
  putStrLn ("=== " ++ name)
  r <- quickCheckWithResult stdArgs { maxSuccess = 200 } p
  pure (isSuccess r)
