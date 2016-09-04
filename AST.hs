{-# LANGUAGE GADTs, StandaloneDeriving, DeriveFunctor, TypeSynonymInstances
  , FlexibleInstances, OverloadedStrings, DeriveTraversable
  , ScopedTypeVariables #-}

module AST where

import Data.Fix
import Data.Fix.Cse
import Data.List hiding (sum)
import Data.String
import GHC.TypeLits
import Prelude hiding (recip, tanh, log, exp, sum, abs)
import qualified Data.IntMap as I
import qualified Data.Map as M

data Matrix
data AstF a where
  Abs :: a -> AstF a
  Add :: a -> a -> AstF a
  Cbind :: a -> a -> AstF a
  ColExpand :: a -> a -> AstF a
  ColSums :: a -> AstF a
  CPad :: a -> a -> a -> a -> a -> AstF a
  CSlice :: a -> a -> a -> AstF a
  Diag :: a -> AstF a
  Exp :: a -> AstF a
  Length :: a -> AstF a
  List :: [a] -> AstF a
  Log :: a -> AstF a
  Mul :: a -> a -> AstF a
  NCol :: a -> AstF a
  Neg :: a -> AstF a
  NRow :: a -> AstF a
  PMul :: a -> a -> AstF a
  Pow :: a -> a -> AstF a
  Rbind :: a -> a -> AstF a
  Recip :: a -> AstF a
  Rep :: a -> a -> AstF a
  RowExpand :: a -> a -> AstF a
  RowSums :: a -> AstF a
  RPad :: a -> a -> a -> a -> a -> AstF a
  RSlice :: a -> a -> a -> AstF a
  Sech2 :: a -> AstF a
  Sum :: a -> AstF a
  Tanh :: a -> AstF a
  Trans :: a -> AstF a
  Var :: String -> AstF a
  deriving Eq

a -. b = a +. neg b
abs = Fix . Abs
cbind = (Fix .) . Cbind
colExpand = (Fix .) . ColExpand
colpad x n m a b = Fix $ CPad x n m a b
colSlice x a b = Fix $ CSlice x a b
colSums = Fix . ColSums
diag = Fix . Diag
exp = Fix . Exp
(+.) = (Fix .) . Add
(*:) = (Fix .) . Mul
(*.) = (Fix .) . PMul
(^.) = (Fix . ) . Pow
len = Fix . Length
list = Fix . List
log = Fix . Log
ncol = Fix . NCol
neg = Fix . Neg
nrow = Fix . NRow
rbind = (Fix .) . Rbind
recip = Fix . Recip
relu x = log ("1" +. exp x)
rep = (Fix .) . Rep
rowExpand = (Fix .) . RowExpand
rowpad x n m a b = Fix $ RPad x n m a b
rowSlice x a b = Fix $ RSlice x a b
rowSums = Fix . RowSums
sech x = "2" *. recip (exp x +. exp (neg x))
sigmoid x = "0.5" *. ("1" +. tanh x)
sum = Fix . Sum
tanh = Fix . Tanh
trans = Fix . Trans
tr = sum . diag
var = Fix . Var

infixl 6 +.
infixl 6 -.
infixl 7 *.
infixl 8 *:
infixl 9 ^.

deriving instance Functor AstF
--deriving instance Eq (Fix AstF)
deriving instance Foldable AstF
deriving instance Traversable AstF
deriving instance Show f => Show (AstF f)
deriving instance Ord f => Ord (AstF f)
type Ast a = Fix AstF

instance Num (Fix AstF) where
  (+) = (+.)
  (-) = (-.)
  (*) = (*.)

instance IsString (Fix AstF) where
  fromString = var

toR = cata toR'
toR' (Abs x) = "abs(" ++ x ++ ")"
toR' (Add a b) = "(" ++ a ++ "+" ++ b ++ ")"
toR' (Cbind a b) = "cbind(" ++ a ++ "," ++ b ++ ")"
toR' (ColExpand a b) = "(rep(1, " ++ b ++ ")%*%t(" ++ a ++ "))"
toR' (ColSums a) = "colSums(" ++ a ++ ")"
toR' (CPad x n m a b) = "cbind(matrix(0," ++ n ++ ", " ++ a ++ " - 1)," ++ x ++ ", matrix(0," ++ n ++ ", " ++ m ++ " - " ++ b ++ "))"
toR' (CSlice x a b) = x ++ "[," ++ a ++ ":" ++ b ++ "]"
toR' (Diag x) = "diag(" ++ x ++ ")"
toR' (Exp a) = "exp(" ++ a ++ ")"
toR' (Length x) = "length(" ++ x ++ ")"
toR' (List xs) = "list(" ++ intercalate "," xs ++ ")"
toR' (Log a) = "log(" ++ a ++ ")"
toR' (Mul a b) = "(" ++ a ++ "%*%" ++ b ++ ")"
toR' (NCol x) = "ncol(" ++ x ++ ")"
toR' (Neg a) = "(-" ++ a ++ ")"
toR' (NRow x) = "nrow(" ++ x ++ ")"
toR' (PMul a b) = "(" ++ a ++ "*" ++ b ++ ")"
toR' (Pow x a) = "(" ++ x ++ "^" ++ a ++ ")"
toR' (Rbind a b) = "rbind(" ++ a ++ "," ++ b ++ ")"
toR' (Recip a) = "(1/" ++ a ++ ")"
toR' (Rep x a) = "rep(" ++ x ++ "," ++ a ++ ")"
toR' (RowExpand a b) = "(" ++ a ++ "%*%t(rep(1, " ++ b ++ ")))"
toR' (RowSums a) = "rowSums(" ++ a ++ ")"
toR' (RPad x n m a b) = "rbind(matrix(0, " ++ a ++ " - 1, " ++ m ++ ")," ++ x ++ ", matrix(0, " ++ n ++ " - " ++ b ++ "," ++ m ++ "))"
toR' (RSlice x a b) = x ++ "[" ++ a ++ ":" ++ b ++ "]"
toR' (Sum x) = "sum(" ++ x ++ ")"
toR' (Tanh x) = "tanh(" ++ x ++ ")"
toR' (Trans a) = "t(" ++ a ++ ")"
toR' (Var x) = x

--instance Show (Fix AstF) where show = toR
notnum str = case reads str of
               [(val :: Double, "")] -> False
               _ -> True

diff = M.map (findFix rewrite) . M.fromListWith (+.) . diff' "1"
diff' dx (Fix (Abs x)) = diff' (dx *. x *. abs x) x
diff' dx (Fix (Add a b)) = diff' dx a ++ diff' dx b
diff' dx (Fix (Cbind a b)) = diff' (colSlice dx "1" (ncol a)) a ++ diff' (colSlice dx ("1" +. ncol a) (ncol a +. ncol b)) b
diff' dx (Fix (ColSums a)) = diff' (colExpand dx (nrow a)) a
diff' dx (Fix (CPad x _ _ a b)) = diff' (colSlice dx a b) x
diff' dx (Fix (CSlice x a b)) = diff' (colpad dx (nrow x) (ncol x) a b) x
diff' dx (Fix (Diag x)) = diff' (diag dx) x
diff' dx (Fix (Exp a)) = diff' (dx *. exp a) a
diff' dx (Fix (Log a)) = diff' (dx *. recip a) a
diff' dx (Fix (Mul a b)) = diff' (dx *: trans b) a ++ diff' (trans a *: dx) b
diff' dx (Fix (Neg a)) = diff' (neg dx) a
diff' dx (Fix (PMul a b)) = diff' (dx *. b) a ++ diff' (dx *. a) b
diff' dx (Fix (Pow x a)) = diff' (dx *. a *. x ^. (a -. "1")) x ++ diff' (dx *. x ^. a *. log x) a
diff' dx (Fix (Rbind a b)) = diff' (rowSlice dx "1" (nrow a)) a ++ diff' (rowSlice dx ("1" +. nrow a) (nrow a +. nrow b)) b
diff' dx (Fix (Recip a)) = diff' (dx *. neg (recip (a *. a))) a
diff' dx (Fix (RowSums a)) = diff' (rowExpand dx (ncol a)) a
diff' dx (Fix (RPad x _ _ a b)) = diff' (rowSlice dx a b) x
diff' dx (Fix (RSlice x a b)) = diff' (rowpad dx (nrow x) (ncol x) a b) x
diff' dx (Fix (Sum x)) = diff' (rep dx (len x)) x
diff' dx (Fix (Tanh x)) = diff' (dx *. sech x *. sech x) x
diff' dx (Fix (Trans a)) = diff' (trans dx) a
diff' dx (Fix (Var x)) = [(x, dx) | notnum x]

findFix f x = if x == x' then x else findFix f x'
  where
    x' = f x

rewrite = cata rewrite'
  where
    rewrite' (Mul a (Fix (Var "1"))) = colSums a
    rewrite' (Mul (Fix (Var "1")) a) = rowSums a
    rewrite' (Trans (Fix (Var "1"))) = "1"
    rewrite' (Trans (Fix (Var "0"))) = "0"
    rewrite' (Trans (Fix (Trans x))) = x
    rewrite' (Add a (Fix (Var "0"))) = a
    rewrite' (Add (Fix (Var "0")) a) = a
    rewrite' (Neg (Fix (Var "0"))) = "0"
    rewrite' (Neg (Fix (Neg x))) = x
    rewrite' (Recip (Fix (Recip x))) = x
    rewrite' x = Fix x

isVar (Var _ ) = True
isVar _ = False

toRFun fn exp = do
  let exp' = cse $ findFix rewrite exp

  -- Determine variables
  let vars = filter notnum $ map (\(Var a) -> a) . I.elems $ I.filter isVar exp'

  putStrLn (fn ++ " <- function(" ++ intercalate "," vars ++ "){")

  -- Generate statements
  mapM_ genIns $ I.toList exp'

  putStrLn "}"

 where
   genIns (id, exp) = putStrLn ("adiffr_s" ++ show id ++ " <- " ++ toR' (tmpName "adiffr_s" exp))
   tmpName prefix = fmap ((prefix++) . show)
