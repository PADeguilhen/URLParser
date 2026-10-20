module URL where

import Parse
import Data.Char (isAlphaNum)


-- | Data

data Schema = HTTPS | HTTP | FTP | ETC String deriving Eq

data Domain = Domain
  -- We shouldn't be able to have empty strings. Whatever ...
  { tdl :: String
  , sdl :: String
  , subDomain :: [String] -- Only the TDL & SDL are mandatory
  } deriving Eq

data Request = Request { param :: String, value :: String } deriving Eq

data URL = URL
  { schema   :: Schema
  , domain   :: Domain
  , path     :: [String]
  , requests :: [Request]
  , fragment :: Maybe String -- if the fragment is present it is not empty 
  } deriving Eq


-- | Show instances

instance Show Schema where
  show HTTPS = "https"
  show HTTP  = "http"
  show FTP   = "ftp"
  show (ETC s) = s

instance Show Domain where
  show Domain{tdl, sdl, subDomain} =
    sdl ++ "." ++ tdl ++ (foldl (\acc s -> acc ++ "." ++ s) "" subDomain)

instance Show Request where
  show Request{param, value} =
    param ++ "=" ++ value

instance Show URL where
  show URL{schema, domain, path, requests, fragment} =
       (show schema)
    ++ "://"
    ++ (show domain)
    ++ (foldl (\acc s -> acc ++ "/" ++ s) "" path)
    ++ (if length requests > 0 then "?" else "")
    ++ (foldl (\acc r -> acc ++ (if length acc == 0 then "" else "&") ++ (show r)) "" requests)
    ++ (case fragment of
          Nothing -> ""
          Just s -> "#" ++ s)


-- | URL Parser

-- Parse a Schema
parseSchema :: Parser Schema
parseSchema = (string "https" >>| \_ -> HTTPS)
          <|> (string "http"  >>| \_ -> HTTP)
          <|> (string "ftp"   >>| \_ -> FTP)
          <|> (take_while1 isAlphaNum >>| \cap -> ETC cap)


notDomainDelimitor :: Char -> Bool
notDomainDelimitor = ( \c -> c /= '.' && isAlphaNum c )

-- Parse a Domain
parseDomain :: Parser Domain
parseDomain =
  (sep_by (string ".") (take_while1 isAlphaNum)) >>= \lstD ->
    case reverse lstD of
      tdl : sdl : xs -> return Domain { tdl = tdl, sdl = sdl, subDomain = reverse xs }
      _ -> failure "domains needs at least a TDL & a SDL"

-- Parse a Path
parsePath :: Parser [String]
parsePath = many (string "/" *> take_while1 isAlphaNum)

-- Parse a Request
parseRequest :: Parser Request
parseRequest =
  lift2 Request
        (take_while1 isAlphaNum <* string "=")
        (take_while1 isAlphaNum)

-- Parse multiple Requests
parseRequests :: Parser [Request]
parseRequests =
  peek >>= \c ->
    case c of
      Just '?' -> string "?" *> (sep_by1 (string "&") parseRequest) <?> "failed to parse Requests"
      _ -> return []

-- Parse a Fragment
parseFragment :: Parser (Maybe String)
parseFragment =
  peek >>= \c ->
    case c of
      Just '#' -> string "#" *> (fmap Just (take_while1 isAlphaNum)) <?> "failed to parse Fragment"
      _ -> return Nothing

-- Parse a full URL
parseURL :: Parser URL
parseURL =
  URL <$> ((parseSchema <?> "failed to parse schema") <* string "://")
      <*> (parseDomain <?> "failed to parse domain")
      <*> (parsePath <?> "failed ot parse path")
      <*> (parseRequests)
      <*> (parseFragment)
