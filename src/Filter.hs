module Filter where

import Data.List (intercalate)
import Parse
import URL
import Data.Char (isAlphaNum)

-- Data

data SchemaFilter = SCHEMA Schema | STAR_SCHEMA

data FullDomainFilter = FullDomainFilter {
    anyStar :: Bool,
    domainFilter :: DomainFilter
}

data DomainFilter = DomainFilter {
    domFilter :: DomainFilterSegment,
    subDomFilter :: DomainFilterSegment,
    segments :: [DomainFilterSegment]
}

data DomainFilterSegment = STAR_DOMAIN | SEGMENT String

data Filter = Filter {
    schemaFilter :: SchemaFilter,
    fullDomainFilter :: FullDomainFilter
}

-- Show

instance Show SchemaFilter where
    show (SCHEMA schema) = show schema
    show STAR_SCHEMA = "*"

instance Show FullDomainFilter where
    show FullDomainFilter {anyStar = True, domainFilter} = "**." ++ show domainFilter
    show FullDomainFilter {anyStar = False, domainFilter} = show domainFilter

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
        (string "**." *> (FullDomainFilter True <$> parseDomainFilter))
    <|> (FullDomainFilter False <$> parseDomainFilter)


parseSegment :: Parser DomainFilterSegment
parseSegment = (string "*" >>| \_ -> STAR_DOMAIN)
           <|> (take_while1 isAlphaNum >>| SEGMENT)

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

filterDomain :: FullDomainFilter -> Domain -> Bool
filterDomain fullDomainFilter domain = undefined

-- data Domain = Domain
--   -- We shouldn't be able to have empty strings. Whatever ...
--   { tdl :: String
--   , sdl :: String
--   , subDomain :: [String] -- Only the TDL & SDL are mandatory
--   } deriving Eq

filterURL :: Filter -> URL -> Bool
filterURL filtre url = filterSchema (schemaFilter filtre) (schema url) && filterDomain (fullDomainFilter filtre) (domain url)