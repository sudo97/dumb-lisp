{-# LANGUAGE OverloadedStrings #-}

module LispAST where

import Control.Monad (when)
import Data.Foldable (foldl')
import qualified Data.Text as T
import JsAST (JSExpr (..), JSStatement (..))

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Double
  | String T.Text
  | Bool Bool
  deriving (Show)

factorialInJSWithTCO :: LispVal
factorialInJSWithTCO = List [Atom "do", List [Atom "define", Atom "fac", funExp]]

funExp :: LispVal
funExp = List [Atom "fun", List [Atom "n"], List [Atom "let", Atom "fac_tailcall", innerFunExp, List [Atom "fac_tailcall", Atom "n", Number 1.0]]]

innerFunExp :: LispVal
innerFunExp = List [Atom "fun", List [Atom "x", Atom "acc"], List [Atom "?", List [Atom "=", Atom "x", Number 1.0], Atom "acc", List [Atom "fac_tailcall", List [Atom "-", Atom "x", Number 1.0], List [Atom "*", Atom "x", Atom "acc"]]]]

type Array a = [a]

toFExprParams :: LispVal -> Maybe (Array T.Text)
toFExprParams (List vs) = traverse toParam vs
  where
    toParam (Atom s) = Just s
    toParam _ = Nothing
toFExprParams _ = Nothing

foldableExpression :: Array LispVal -> (JSExpr -> JSExpr -> JSExpr) -> Maybe JSExpr
foldableExpression vs f = do
  exprs <- traverse toJSExpr vs
  let (firstItem : restItems) = exprs
  pure (foldl' f firstItem restItems)

toJSExpr :: LispVal -> Maybe JSExpr
toJSExpr (List atoms) = do
  let (x : xs) = atoms
  case x of
    (Atom "fun") -> do
      let [params, body] = xs
      body' <- toJSExpr body
      params' <- toFExprParams params
      pure $ FExpr params' [Return body']
    (Atom "let") -> do
      let [Atom name, init, expr] = xs
      expr' <- toJSExpr expr
      case toTailRecursive name init of
        Just fexpr' ->
          pure $
            FCall
              ( FExpr
                  []
                  [ Const name fexpr',
                    Return expr'
                  ]
              )
              []
        Nothing -> do
          init' <- toJSExpr init
          expr' <- toJSExpr expr
          pure $ FCall (FExpr [] [Const name init', Return expr']) []
    (Atom "+") -> foldableExpression xs Add
    (Atom "-") -> foldableExpression xs Sub
    (Atom "*") -> foldableExpression xs Mul
    (Atom "/") -> foldableExpression xs Div
    (Atom "?") -> do
      let [cond, ifTrue, ifFalse] = xs
      Ternary <$> toJSExpr cond <*> toJSExpr ifTrue <*> toJSExpr ifFalse
    (Atom "=") -> do
      let [a, b] = xs
      a' <- toJSExpr a
      b' <- toJSExpr b
      pure $ Eq a' b'
    (Atom ">") -> do
      let [a, b] = xs
      a' <- toJSExpr a
      b' <- toJSExpr b
      pure $ Gt a' b'
    (Atom "<") -> do
      let [a, b] = xs
      a' <- toJSExpr a
      b' <- toJSExpr b
      pure $ Lt a' b'
    (Atom "<=") -> do
      let [a, b] = xs
      a' <- toJSExpr a
      b' <- toJSExpr b
      pure $ Le a' b'
    (Atom ">=") -> do
      let [a, b] = xs
      a' <- toJSExpr a
      b' <- toJSExpr b
      pure $ Ge a' b'
    (Atom "not") -> do
      let [a] = xs
      a' <- toJSExpr a
      pure $ Not a'
    (Atom "and") -> foldableExpression xs And
    (Atom "or") -> foldableExpression xs Or
    (Atom s) -> FCall (Var s) <$> traverse toJSExpr xs
    _ -> Nothing
toJSExpr (Atom s) = Just $ Var s
toJSExpr (Number n) = Just $ JSNumber n
toJSExpr (String s) = Just $ JSString s
toJSExpr (Bool b) = Just $ JSBoolean b

toTailRecursive :: T.Text -> LispVal -> Maybe JSExpr
toTailRecursive fName (List [Atom "fun", List args, expr]) = case expr of
  List [Atom "?", cond, List (Atom n : resets), final] | n == fName -> do
    let args' = map (\(Atom s) -> s) args
    cond' <- toJSExpr cond
    final' <- toJSExpr final
    resets <- traverse toJSExpr resets
    pure $
      FExpr
        args'
        [ WhileLoop
            (JSBoolean True)
            [ If
                cond'
                [ Reset args' resets,
                  Continue
                ],
              Return final'
            ]
        ]
  List [Atom "?", cond, final, List (Atom n : resets)] | n == fName -> do
    let args' = map (\(Atom s) -> s) args
    cond' <- toJSExpr cond
    final' <- toJSExpr final
    resets <- traverse toJSExpr resets
    pure $
      FExpr
        args'
        [ WhileLoop
            (JSBoolean True)
            [ If
                cond'
                [Return final'],
              Reset args' resets
            ]
        ]
  _ -> Nothing
toTailRecursive _ _ = Nothing

translate :: LispVal -> Maybe [JSStatement]
translate (List vs) = do
  let (x : xs) = vs
  case x of
    (Atom "do") -> traverse toJSStatement xs
    _ -> Nothing
translate _ = Nothing

toJSStatement :: LispVal -> Maybe JSStatement
toJSStatement (List vs) = do
  let (x : xs) = vs
  case x of
    (Atom "define") -> do
      let (definedItem : rest) = xs
      case definedItem of
        (Atom name) | length rest == 1 -> do
          let expr = head rest
          case toTailRecursive name expr of
            Just fexpr' -> pure $ Const name fexpr'
            Nothing -> Const name <$> toJSExpr expr
        _ -> Nothing
    _ -> ExprStatement <$> toJSExpr (List vs)
toJSStatement s = ExprStatement <$> toJSExpr s