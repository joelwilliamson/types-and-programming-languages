
type Info = ()

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term
            deriving (Eq,Show)

-- A numerical value is a fully simplified number. If there are any TmPred, it
-- has not been fully simplified
isNumericalVal :: Term -> Bool
isNumericalVal (TmZero _) = True
isNumericalVal (TmSucc _ t1) = isNumericalVal t1
isNumericalVal _ = False

isVal :: Term -> Bool
isVal (TmTrue _) = True
isVal (TmFalse _) = True
isVal t = isNumericalVal t

eval :: Term -> Term
eval (TmIf _ c t f) = case eval c of
  -- B-IfTrue
  TmTrue _ -> eval t
  -- B-IfFalse
  TmFalse _ -> eval f
  _ -> error $ "Couldn't reduce " ++ show c ++ " to a boolean"
-- B-Succ
eval (TmSucc i t) = TmSucc i $ eval t
eval (TmPred _ t) = case eval t of
  -- B-PredZero
  TmZero i -> TmZero i
  -- B-PredSucc
  TmSucc _ n -> n
  _ -> error $ "Couldn't reduce " ++ show t ++ "to a numeric value"
eval (TmIsZero i t) = case eval t of
  -- B-IsZeroZero
  TmZero _ -> TmTrue i
  -- B-IsZeroSucc
  _ -> if isNumericalVal t
       then TmFalse i
       else error $ show t ++ " is not a number"
-- B-Value
eval t = t
