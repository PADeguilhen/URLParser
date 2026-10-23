module Filter where

import Data.List (intercalate)
import Parse
import URL
import Data.Char (isAlphaNum)

-- Data

data SchemaFilter = SCHEMA Schema | STAR_SCHEMA deriving Eq

data FullDomainFilter = WILDCARD_DOMAIN_FILTER [DomainFilterSegment] | DOMAIN_FILTER DomainFilter deriving Eq

data DomainFilter = DomainFilter {
    domFilter :: DomainFilterSegment,
    subDomFilter :: DomainFilterSegment,
    segments :: [DomainFilterSegment]
} deriving Eq

data DomainFilterSegment = STAR_DOMAIN | SEGMENT String deriving Eq

data Filter = Filter {
    schemaFilter :: SchemaFilter,
    fullDomainFilter :: FullDomainFilter
} deriving Eq

-- Show

instance Show SchemaFilter where
    show (SCHEMA schema) = show schema
    show STAR_SCHEMA = "*"

instance Show FullDomainFilter where
    show (WILDCARD_DOMAIN_FILTER domainFilterSegments) = "**." ++ intercalate "." (map show domainFilterSegments)
    show (DOMAIN_FILTER domainFilter) = show domainFilter

instance Show DomainFilter where
    show DomainFilter {domFilter, subDomFilter, segments = []} = show subDomFilter ++ "." ++ show domFilter
    show DomainFilter {domFilter, subDomFilter, segments} = intercalate "." (map show segments) ++ "." ++ show subDomFilter ++ "." ++ show domFilter

instance Show DomainFilterSegment where
    show STAR_DOMAIN = "*"
    show (SEGMENT str) = str

instance Show Filter where
    show Filter {schemaFilter, fullDomainFilter} = show schemaFilter ++ "://" ++ show fullDomainFilter

-- Parsing

parseFilter :: Parser Filter
parseFilter =
    Filter <$> ((parseSchemaFilter <?> "failed to parse schema") <* string "://")
      <*> (parseFullDomainFilter <?> "failed to parse full domain filter")

parseSchemaFilter :: Parser SchemaFilter
parseSchemaFilter = (string "https" >>| \_ -> SCHEMA HTTPS)
          <|> (string "http"  >>| \_ -> SCHEMA HTTP)
          <|> (string "ftp"   >>| \_ -> SCHEMA FTP)
          <|> (string "*"     >>| \_ -> STAR_SCHEMA)
          <|> (take_while1 isAlphaNum >>| \cap -> SCHEMA (ETC cap))

parseFullDomainFilter :: Parser FullDomainFilter
parseFullDomainFilter =
        (string "**" *> (WILDCARD_DOMAIN_FILTER <$> (string "." *> parseListDomainFilter <|> pure [])))
    <|> (DOMAIN_FILTER <$> parseDomainFilter)


parseSegment :: Parser DomainFilterSegment
parseSegment = (string "*" >>| \_ -> STAR_DOMAIN)
           <|> (take_while1 isAlphaNum >>| SEGMENT)

parseListDomainFilter :: Parser [DomainFilterSegment]
parseListDomainFilter = sep_by (string ".") parseSegment

parseDomainFilter :: Parser DomainFilter
parseDomainFilter = sep_by (string ".") parseSegment >>= \lstD ->
    case reverse lstD of
      tdl : sdl : xs -> return DomainFilter
          { domFilter    = tdl
          , subDomFilter = sdl
          , segments     = reverse xs
          }
      _ -> failure "domains needs at least a TDL & a SDL"

-- Filtering

filterSchema :: SchemaFilter -> Schema -> Bool
filterSchema STAR_SCHEMA _ = True
filterSchema (SCHEMA schemaFilter) schema = schemaFilter == schema

convertDomain :: Domain -> [String]
convertDomain domain = (tdl domain) : (sdl domain) : (reverse (subDomain domain))

convertDomainFilter :: DomainFilter -> [DomainFilterSegment]
convertDomainFilter domain = (domFilter domain) : (subDomFilter domain) : (reverse (segments domain))

filterDomain :: FullDomainFilter -> Domain -> Bool
filterDomain (WILDCARD_DOMAIN_FILTER domainsFilter) domains = matchListSegments (reverse domainsFilter) (convertDomain domains) True
filterDomain (DOMAIN_FILTER domainsFilter) domains = matchListSegments (convertDomainFilter domainsFilter) (convertDomain domains) False

matchSegment :: DomainFilterSegment -> String -> Bool
matchSegment STAR_DOMAIN _ = True
matchSegment (SEGMENT s1) s2 = s1 == s2

matchListSegments :: [DomainFilterSegment] -> [String] -> Bool -> Bool
matchListSegments domainFilterSegments domainsSegments wildcard = case (domainFilterSegments, domainsSegments) of
    ([], []) -> True
    ([], _) -> wildcard
    (_, []) -> False
    (d1 : q1, d2 : q2) -> matchSegment d1 d2 && matchListSegments q1 q2 wildcard

filterURL :: Filter -> URL -> Bool
filterURL filtre url = filterSchema (schemaFilter filtre) (schema url) && filterDomain (fullDomainFilter filtre) (domain url)

filterURLString :: String -> String -> Bool
filterURLString filterStr urlStr =
    case (parseStr parseFilter filterStr, parseStr parseURL urlStr) of
        (Right filterParsed, Right urlParsed) -> filterURL filterParsed urlParsed
        _ -> False