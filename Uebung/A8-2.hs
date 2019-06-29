import Prelude hiding (lex, const)
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup -- für ältere GHC Versionen notwendig

------------------
-- Aufgabe A8-2 --
------------------

type Name = String
data Expr = Var String
          | Const Double
          | Plus Expr Expr
          | Times Expr Expr
  deriving (Eq)

-- Im Folgenden sind Show-Instanz und parse-Funktion definiert.
-- Die eigentliche Aufgabe beginnt auf Zeile 74.

instance Show Expr where
  show (Var x) = x
  show (Const i) = show i
  show (Plus e1 e2) = "("++ show e1 ++ "+" ++ show e2 ++")"
  show (Times e1 e2) = "("++ show e1 ++ "*" ++ show e2 ++")"

data Token = ID Name | CONST Double | LPAREN | RPAREN | PLUS | TIMES
  deriving Show 

-- Lexikalische Analyse
lex :: String -> [Token]
lex "" = []
lex (c:s) | isSpace c = lex s
lex ('(':s) = LPAREN : lex s
lex (')':s) = RPAREN : lex s
lex ('[':s) = LPAREN : lex s
lex (']':s) = RPAREN : lex s
lex ('+':s) = PLUS   : lex s
lex ('*':s) = TIMES  : lex s
lex (c:s) | isAlpha c = ID x : lex rest
  where (x, rest) = break (not . isAlphaNum) (c:s) 
lex s | [(i, rest)] <- reads s = CONST i : lex rest
lex s = error $ "Unbekanntes Zeichen: " <> s

-- Syntaxanalyse, Folie 7.31ff.
parseExpr   :: [Token] -> (Expr, [Token])
parseExpr l = case parseProd l of 
                (prod, PLUS:rest1) -> 
                  let (expr,rest2) = parseExpr rest1
                  in (Plus prod expr, rest2)
                (prod, rest) -> (prod, rest)

parseProd   :: [Token] -> (Expr, [Token])
parseProd l = case parseFactor l of 
                (factor, TIMES:rest1) -> 
                  let (expr, rest2) = parseFactor rest1
                  in (Times factor expr, rest2)
                (factor, rest) -> (factor, rest)

parseFactor :: [Token] -> (Expr, [Token])
parseFactor ((ID x):rest) = (Var x, rest)
parseFactor ((CONST i):rest) = (Const i, rest)
parseFactor (LPAREN:rest) = case parseExpr rest of
                              (expr, RPAREN:rest2) -> (expr, rest2)
                              _ -> error "Schließende Klammer erwartet."
parseFactor _ = error "Syntaxfehler! Variable, Konstante oder öffnende Klammer erwartet."                             
  
parse :: String -> Expr
parse str = case parseExpr (lex str) of 
              (expr, []) -> expr 
              (_,bad) -> error $ "Unnötiger Ballast am Ende: " ++ show bad

-- a) Auswertung
--eval :: Map Name Double -> Expr -> Maybe Double
-- eval a (Var x) = Map.lookup x a
-- eval a (Var x) =
-- eval a (Var x) =

-- b) Substitution
subst :: (Name -> Expr) -> Expr -> Expr
subst f (Var x) = f x
subst f (Const n) = Const n
subst f (Plus l r) =


-- c) Vereinfachung
simplify :: Expr -> Expr


var :: Name -> Expr
var name = Var (show name)

const :: Double -> Expr
const n = error "TODO"

plus :: Expr -> Expr -> Expr
plus = error "TODO"

times :: Expr -> Expr -> Expr
times = error "TODO"

-- d)
foldExpr :: (Name -> a) -> (Double -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr var const plus times exp = fold exp
  where fold (Var x) = var x
        fold (Const c) = const c
        fold (Plus l r) = plus (fold l) (fold r)
        fold (Times l r) = times (fold l) (fold r) 


simplify' :: Expr -> Expr
simplify' = foldExpr var const plus times

eval' :: Map Name Double -> Expr -> Maybe Double
eval' m = foldExpr (error "TODO") (error "TODO") (error "TODO") (error "TODO")

subst' :: (Name -> Expr) -> Expr -> Expr
subst' f = foldExpr (error "TODO") (error "TODO") (error "TODO") (error "TODO")

