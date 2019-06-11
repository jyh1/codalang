{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Lang.Parser
    ( loadFile
    , loadString
    , codaParser
    , byteToUUID
    )
where

import           Lang.Types

import           RIO                     hiding ( try )
import qualified RIO.Text                      as T
import qualified RIO.Map                       as M
-- import           Text.Parser.Combinators        ( (<?>) )
import           Text.Parser.Combinators
-- import Text.Parser.Expression
import           Text.Parser.Token
-- import           Text.Parser.Char
import           Text.Parser.Char               ( char )
-- import           Text.Parser.Token.Style        ( emptyOps )
import           Text.Parser.Token.Highlight
import           Text.Trifecta
import           Control.Lens                   ( _2 )

data ParseRes = PLit Text
    | PVar Text
    | PStr Text
    | PLet [(Text, ParseRes)] ParseRes
    | PRun [ParseRes]
    | PDir ParseRes [Text]
    | PConv ParseRes [CodaType]
    | PDict (Map Text ParseRes)
    deriving (Show, Read, Eq, Ord)

fromParseRes :: ParseRes -> CodaVal
fromParseRes res = case res of
    PLit n       -> Lit (UUID n)
    PVar v       -> Var v
    PStr s       -> Str s
    PLet as body -> foldr (uncurry Let)
                          (fromParseRes body)
                          (over (traverse . _2) fromParseRes as)
    PRun ps   -> Cl (Run (fromParseRes <$> ps))
    PDir home subs -> foldl' Dir (fromParseRes home) subs
    PConv val ts -> case ts of
        [] -> fromParseRes val
        _ -> foldl' Convert (fromParseRes val) ts
    PDict d -> Dict (fromParseRes <$> d)

optionalFollowed :: (a -> b -> a) -> a -> Maybe b -> a
optionalFollowed f a m = case m of
    Nothing -> a
    Just b -> f a b

fileNameChar :: (TokenParsing m) => m Char
fileNameChar = noneOf "/\\?%*:|\"><';(),{}[]\r\n\t "

parseDicSyntax :: (TokenParsing m) => m a -> m b -> m [(a, b)]
parseDicSyntax key val = braces eles
    where
        ele = liftA2 (curry id) (key <* token (symbol ":")) val
        eles = sepBy ele (symbol ",")

bundleLit :: (TokenParsing m) => m ParseRes
bundleLit = highlight Constant uuidlit
    where 
        bundleName = T.pack <$> (char '0' *> char 'x' *> some hexDigit)
        uuidlit = token ((PLit <$> bundleName) <?> "UUID or bundle name")

varChar :: (TokenParsing m) => m Char
varChar = alphaNum <|> oneOf "_.-"

varName :: (TokenParsing m) => m Text
varName = highlight Identifier (T.pack <$> varname <?> "variable_name")
    where varname = token (liftA2 (:) letter (many varChar))

varExpr :: (TokenParsing m) => m ParseRes
varExpr = PVar <$> varName <?> "variable"

makeKeyword :: (TokenParsing m) => String -> m String
makeKeyword t = try
    (   highlight ReservedIdentifier (token (string t <* notFollowedBy varChar))
    <?> t
    )

letExpr :: (TokenParsing m) => m ParseRes
letExpr = highlight Constructor (token (expr <?> "let_exprssions"))
  where
    letkey = makeKeyword "let"
    inkey  = makeKeyword "in"
    expr =
        liftA2 PLet (letkey *> sepEndBy (try letStmt) semi <* inkey) codaExpr

    letStmt :: (TokenParsing m) => m (Text, ParseRes)
    letStmt = highlight Statement stmt <?> "let statement"
        where stmt = token (liftA2 (,) varName (symbolic '=' *> codaExpr))

-- | used to build dir expression and paren expression
followedByList
    :: (TokenParsing m) => m a -> m b -> m c -> m (a, Maybe [c])
followedByList p sep after = token $ do
    e1   <- p
    rest <-
        (Just <$> (sep *> sepBy after sep)) <|> pure Nothing
    return (e1, rest)

-- | A run bundle or (expr)
-- | run bundle: (e1, ) (e1, e2, ...)
-- | expr: (e)
parenExpr :: (TokenParsing m) => m ParseRes
parenExpr = highlight Special (token (parens inside)) <?> "paren expression"
  where
    inside = uncurry makeRun <$> followedByList codaExpr comma codaExpr
    makeRun :: ParseRes -> Maybe [ParseRes] -> ParseRes
    makeRun e1 Nothing   = e1
    makeRun e1 (Just es) = PRun (e1 : es)

stringExpr :: (TokenParsing m) => m ParseRes
stringExpr = PStr <$> stringLiteral

-- | e/sub1/sub2/file1
dirExpr :: (TokenParsing m) => m ParseRes
dirExpr = highlight LiterateSyntax (token dirWithType) <?> "codalang expression"
  where
    filename = T.pack <$> many fileNameChar <?> "file name"
    dirSep   = highlight ReservedOperator (char '/' <?> "path seperator")
    makeDir :: (ParseRes, Maybe [Text]) -> ParseRes
    makeDir = uncurry (optionalFollowed PDir)
    dirParse = token (makeDir <$> followedByList normalExpr dirSep filename)
    dirWithType = liftA2 PConv dirParse (many typeAnnotation)

normalExpr :: (TokenParsing m) => m ParseRes
normalExpr =
    (bundleLit <|> stringExpr <|> letExpr <|> varExpr <|> parenExpr <|> dictExpr)
        <?> "regular codalang expression"


dictKey :: (TokenParsing m) => m Text
dictKey = T.pack <$> (some fileNameChar) <?> "type dictionary key"

typeAnnotation :: (TokenParsing m) => m CodaType
typeAnnotation = hasAnnot <?> "type annotation"
    where
        hasAnnot = symbol "::" *> typeExpr
        typeExpr :: (TokenParsing m) => m CodaType
        typeExpr = typeStr <|> typeBunDict <?> "type expression"
            where
                typeStr = makeKeyword "String" $> TypeString
                typeBun = TypeRecord . M.fromList <$> parseDicSyntax (token dictKey) (token typeExpr)
                typeBunAll = braces (symbol "_") $> TypeBundle
                typeBunDict = try typeBunAll <|> typeBun

dictExpr :: (TokenParsing m) => m ParseRes
dictExpr = do
    let dict = M.fromList <$> parseDicSyntax (token dictKey) codaExpr
    PDict <$> dict <?> "dictionary"

codaExpr :: (TokenParsing m) => m ParseRes
codaExpr = dirExpr

codaParser :: Parser CodaVal
codaParser = fromParseRes <$> (spaces *> codaExpr)

loadFile :: (MonadIO m) => String -> m (Maybe CodaVal)
loadFile = parseFromFile codaParser

loadString :: String -> Maybe CodaVal
loadString inp = case parseString codaParser mempty inp of
    Success a -> Just a
    Failure _ -> Nothing

-- parse UUID from command line output
uuidParser :: Parser UUID
uuidParser = do
    u <- bundleLit
    case u of
        PLit x -> pure (UUID x)
        _ -> unexpected "not a uuid"

byteToUUID :: ByteString -> Maybe UUID
byteToUUID inp = case parseByteString uuidParser mempty inp of
    Success a -> Just a
    Failure _ -> Nothing