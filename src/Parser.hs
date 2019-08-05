{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser (parseProblem, convertProblem, ProblemDescription) where

import Text.Megaparsec (Parsec, try, notFollowedBy, between, eof, parse, errorBundlePretty, sepBy1, endBy, some)
import Text.Megaparsec.Char (space1, string, letterChar, alphaNumChar, char)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (many, (<|>))
import Data.List (concat, elemIndex)
import Data.Void (Void)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Syntax as S
import Data.Map.Strict (Map)
import Control.Monad (MonadPlus(mplus), forM_)
import qualified Data.Map.Strict as M
import Name
import TCMonad

data ProblemHeader = ProblemHeader
  { problemTypes :: [Text]
  , problemConstants :: [(Text, S.Ty)]
  , problemMetas :: [(Text, S.Ty)]
  }
  deriving (Eq, Ord, Show)

data ProblemDescription = ProblemDescription
  { problemHeader :: ProblemHeader
  , problemEquations :: [(Term, Term)]
  }
  deriving (Eq, Ord, Show)

data Term = Var Text
          | Lambda Text S.Ty Term
          | App Term Term
          deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where lineComment = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol x = L.symbol sc x >> pure ()

rword :: Text -> Parser ()
rword w = lexeme (try (string w *> notFollowedBy alphaNumChar))

reservedWords :: [Text]
reservedWords = ["types", "constants", "metavariables", "equations", "fun"]

identifier :: Parser Text
identifier = lexeme (try (name >>= check))
  where name = T.cons <$> letterChar <*> (T.pack <$> many (alphaNumChar <|> char '\''))
        check x = if x `elem` reservedWords
                  then fail $ "keyword " ++ show x ++ " cannot be used as an identifier"
                  else return x

binders :: Parser [(Text, S.Ty)]
binders = manybinders <|> singlebinder

manybinders :: Parser [(Text, S.Ty)]
manybinders = do
  groups <- some (parens singlebinder)
  return (concat groups)

singlebinder :: Parser [(Text, S.Ty)]
singlebinder = do
  idents <- identifier `sepBy1` sc
  symbol ":"
  ty <- type_
  return [ (i, ty) | i <- idents ]

term :: Parser Term
term = makeExprParser term'
  [ [ InfixL (App <$ sc) ] ]

term' :: Parser Term
term' = var
    <|> lambda
    <|> parens term

var :: Parser Term
var = Var <$> identifier

lambda :: Parser Term
lambda = do
  rword "fun"
  bs <- binders
  symbol "=>"
  body <- term
  pure (foldr (\(name, ty) tm -> Lambda name ty tm) body bs)

baseType :: Parser S.Ty
baseType = S.BaseTy <$> identifier

type_ :: Parser S.Ty
type_ = makeExprParser (baseType <|> parens type_)
  [ [ InfixR ((S.:->) <$ symbol "->") ] ]

problem :: Parser ProblemDescription
problem = ProblemDescription <$> header <*> equations

header :: Parser ProblemHeader
header = ProblemHeader <$> types <*> constants <*> metavariables

types :: Parser [Text]
types = do
  rword "types"
  identifier `endBy` symbol ";"

decl :: Parser (Text, S.Ty)
decl = do
  name <- identifier
  symbol ":"
  ty <- type_
  return (name, ty)

decls :: Parser [(Text, S.Ty)]
decls = decl `endBy` symbol ";"

metavariables :: Parser [(Text, S.Ty)]
metavariables = do
  rword "metavariables"
  decls

constants :: Parser [(Text, S.Ty)]
constants = do
  rword "constants"
  decls

equation :: Parser (Term, Term)
equation = do
  t1 <- term
  symbol "=="
  t2 <- term
  return (t1, t2)

equations :: Parser [(Term, Term)]
equations = do
  rword "equations"
  equation `endBy` symbol ";"

parseProblem :: Text -> Either Text ProblemDescription
parseProblem source = case parse (between sc eof problem) "<interactive>" source of
  Left e -> Left (T.pack (errorBundlePretty e))
  Right t -> Right t

lookupName :: Map Text S.Term -> [Text] -> Text -> Maybe S.Term
lookupName topLevel locals name = lookupTopLevel `mplus` lookupLocal
  where lookupTopLevel = M.lookup name topLevel
        lookupLocal = (\i -> S.Var (Bound i ())) <$> elemIndex name locals

convertTerm :: [Text] -> Map Text S.Term -> Term -> TC S.Term
convertTerm declaredTypes topLevel = go []
  where go :: [Text] -> Term -> TC S.Term
        go locals (App t1 t2) = (S.:@) <$> go locals t1 <*> go locals t2
        go locals (Lambda name ty body) = do
          checkTy declaredTypes ty
          S.Abs <$> (ManualScope (Ignore name, ty) <$> go (name:locals) body)
        go locals (Var name) = case lookupName topLevel locals name of
          Just x -> return x
          Nothing -> typeError (ScopeError "variable out of scope")

checkTy :: [Text] -> S.Ty -> TC ()
checkTy declaredTypes (t1 S.:-> t2) = checkTy declaredTypes t1 >> checkTy declaredTypes t2
checkTy declaredTypes (S.BaseTy tyname)
    | tyname `elem` declaredTypes = return ()
    | otherwise = typeError . ScopeError $ "unknown type"

convertProblem :: ProblemDescription -> TC [(S.Term, S.Term)]
convertProblem desc = do
  let hdr = problemHeader desc
  let metaVars = problemMetas hdr
  let consts = problemConstants hdr
  let knownTypes = problemTypes hdr

  forM_ consts $ \(_, ty) -> checkTy knownTypes ty
  forM_ metaVars $ \(_, ty) -> checkTy knownTypes ty

  -- TODO: Check for duplicates between constants and metavariables

  -- TODO: the following line is kind of ugly
  freshMetaVars <- mapM (\(name, ty) -> (\m -> (name, S.Meta (S.MetaVar m))) <$> fresh ty) metaVars
  let termConstants = map (\(name, ty) -> (name ,S.Const (S.C name ty))) consts
  let topLevel = M.union (M.fromList freshMetaVars) (M.fromList termConstants)

  let eqs = problemEquations desc
  mapM (\(t1, t2) -> (,) <$> convertTerm knownTypes topLevel t1 <*> convertTerm knownTypes topLevel t2) eqs
