{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}

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
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Char               ( char )
import           Text.Parser.Token.Highlight
import           Text.Trifecta

data PAssign = PLetVar Text | PLetOpt Text | PLetFun Text TypeDict
    deriving (Show, Read, Eq, Ord)

data ParseRes = PLit Text
    | PVar Text
    | PStr Text
    | PLet [(PAssign, ParseRes)] ParseRes
    | PRun [ParseRes]
    | PDir ParseRes Text
    | PConv ParseRes [CodaType]
    | PDict (Map Text ParseRes)
    | PApply ParseRes (TextMap ParseRes)
    | PLoad Module
        deriving (Show, Read, Eq, Ord) 


fromPLet :: (LoadModule m) => PAssign -> ParseRes -> m CodaVal -> m CodaVal
fromPLet pa val body = do
    let var = case pa of
            PLetVar t -> Variable t
            PLetOpt t -> OptionVar t
            PLetFun f _ -> Variable f
    valres <- fromParseRes val
    let letval = case pa of
            PLetFun _ args -> Lambda args valres
            _ -> valres
    Let var letval <$> body


fromParseRes :: (LoadModule m) => ParseRes -> m CodaVal
fromParseRes res = case res of
    PLit n       -> return $ Lit (UUID n)
    PVar v       -> return $ Var v
    PStr s       -> return $ Str s
    PLet as body -> foldr (uncurry fromPLet)
                          (fromParseRes body)
                          as
    PRun ps   -> (makeCl . Run) <$> (mapM fromParseRes ps)
    PDir home subs -> (`Dir` subs) <$> fromParseRes home
    PConv val ts -> case ts of
        [] -> fromParseRes val
        _ -> do
            valres <- fromParseRes val
            return (foldl' defConvert valres ts)
    PDict d -> Dict <$> (mapM fromParseRes d)
    PApply fun args -> liftM2 Apply (fromParseRes fun) (mapM fromParseRes args)
    PLoad src -> do
        loadModule src
        undefined

optionalFollowed :: (a -> b -> a) -> a -> Maybe b -> a
optionalFollowed f a m = case m of
    Nothing -> a
    Just b -> f a b

fileNameChar :: (TokenParsing m) => m Char
fileNameChar = noneOf "/\\?%*:|\"><';(),{}[]\r\n\t "

parseDicSyntax :: (TokenParsing m) => m a -> m b -> m [(a, b)]
parseDicSyntax k v = braces (parseDicEles k v)

parseDicEles :: (TokenParsing m) => m a -> m b -> m [(a, b)]
parseDicEles key val = eles
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

    letStmt :: (TokenParsing m) => m (PAssign, ParseRes)
    letStmt = highlight Statement stmt <?> "let statement"
        where 
            stmt = token (liftA2 (,) assignable (symbolic '=' *> codaExpr))
            globalVar :: (TokenParsing m) => m PAssign
            globalVar = PLetOpt <$> (text "--" *> varName)
            funVar :: (TokenParsing m) => m PAssign
            funVar = liftA2 consAssign (token varName) (optional $ typeDictExpr brackets)
                where consAssign v = maybe (PLetVar v) (PLetFun v)
            assignable = globalVar <|> funVar

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

-- | e/sub1/sub2[...]/file1[...]
suffixExpr :: (TokenParsing m) => m ParseRes
suffixExpr = highlight LiterateSyntax (token suffixWithType) <?> "codalang expression"
  where
    filename :: (TokenParsing m) => m Text
    filename = T.pack <$> many fileNameChar <?> "file name"
    dirSep :: (TokenParsing m) => m Char
    dirSep   = highlight ReservedOperator (char '/' <?> "path seperator")
    parseDir, parseApply :: (TokenParsing m) => m (ParseRes -> ParseRes)
    parseDir = (flip PDir) <$> (dirSep *> filename)
    parseApply = (flip PApply) <$> (dictExpr brackets)
    praseSuffix = parseDir <|> parseApply
    applySuffixes e fs = foldr ($) e (reverse fs)
    suffixParse = token (liftA2 applySuffixes normalExpr (many praseSuffix))
    suffixWithType = liftA2 PConv suffixParse (many typeAnnotation)


normalExpr :: (TokenParsing m) => m ParseRes
normalExpr =
    (bundleLit <|> stringExpr <|> letExpr <|> varExpr <|> parenExpr <|> pdictExpr <|> loadExpr)
        <?> "regular codalang expression"


dictKey :: (TokenParsing m) => m Text
dictKey = T.pack <$> (some fileNameChar) <?> "type dictionary key"

typeAnnotation :: (TokenParsing m) => m CodaType
typeAnnotation = hasAnnot <?> "type annotation"
    where
        hasAnnot = makeKeyword "as" *> typeExpr

typeExpr :: (TokenParsing m) => m CodaType
typeExpr = typeStr <|> typeBunDict <|> typeFun <?> "type expression"
    where
        typeStr = makeKeyword "string" $> TypeString
        typeBun = TypeRecord <$> (typeDictExpr braces)
        typeBunAll = makeKeyword "bundle" $> TypeBundle
        typeBunDict = typeBunAll <|> typeBun 
        typeFun = liftA2 TypeLam (token (typeDictExpr brackets)) (token (symbol "=>") *> typeExpr)

typeDictExpr :: (TokenParsing m) => (forall a. m a -> m a) -> m TypeDict
typeDictExpr bra = M.fromList <$> bra (parseDicEles (token dictKey) (token typeExpr))

dictExpr :: (TokenParsing m) => (forall a. m a -> m a) -> m (TextMap ParseRes)
dictExpr bra = do
    let dict = M.fromList <$> bra (parseDicEles (token dictKey) codaExpr)
    dict <?> "dictionary"

pdictExpr :: (TokenParsing m) => m ParseRes
pdictExpr = PDict <$> (dictExpr braces)

loadExpr :: (TokenParsing m) => m ParseRes
loadExpr = token (token (text "%load") *> (PLoad <$> modExpr))
    where
        modExpr = liftA2 ($) scheme pathComponent

scheme :: (TokenParsing m) => m (Text -> Module)
scheme = url <|> file <|> (pure CodaBundle)
    where
        makeURL sch l = URL (sch <> "://" <> l)
        url =         
            (text "http")
            *>  ((text "s" *> pure (makeURL "https")) <|> pure (makeURL "http")) 
            <* (text "://")
        file = (\pref l -> SysPath (pref <> l)) <$> (text "./" <|> text "/")

pathComponent :: (TokenParsing m) => m Text
pathComponent = T.pack <$> many (satisfy (\c -> c /= ';' && pathCharacter c) <|> char '/')

pathCharacter :: Char -> Bool
pathCharacter c =
         '\x21' == c
    ||  ('\x24' <= c && c <= '\x27')
    ||  ('\x2A' <= c && c <= '\x2B')
    ||  ('\x2D' <= c && c <= '\x2E')
    ||  ('\x30' <= c && c <= '\x3B')
    ||  c == '\x3D'
    ||  ('\x40' <= c && c <= '\x5A')
    ||  ('\x5E' <= c && c <= '\x7A')
    ||  c == '\x7C'
    ||  c == '\x7E'

codaExpr :: (TokenParsing m) => m ParseRes
codaExpr = token (suffixExpr) <* eof

codaParser :: Parser ParseRes
codaParser = spaces *> codaExpr

loadFile :: (LoadModule m) => String -> m CodaVal
loadFile f = parseModule (SysPath (T.pack f))

parseModule :: (LoadModule m) => Module -> m CodaVal
parseModule mdl = do
    s <- loadModule mdl
    fromResult (parseByteString codaParser mempty s)

loadString :: (LoadModule m) => String -> m CodaVal
loadString inp = fromResult (parseString codaParser mempty inp)

fromResult :: (LoadModule m) => Result ParseRes -> m CodaVal
fromResult res = case res of
    Success a -> fromParseRes a
    Failure xs -> parseError (show (_errDoc xs))

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