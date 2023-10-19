{-# LANGUAGE OverloadedStrings #-}

module JsAST where

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data JSExpr
  = FCall JSExpr [JSExpr]
  | JSNumber Double
  | JSString T.Text
  | JSBoolean Bool
  | JSArray [JSExpr]
  | JSObject [(T.Text, JSExpr)]
  | Ternary JSExpr JSExpr JSExpr
  | FExpr [T.Text] [JSStatement]
  | Var T.Text
  | Mul JSExpr JSExpr
  | Add JSExpr JSExpr
  | Sub JSExpr JSExpr
  | Div JSExpr JSExpr
  | Gt JSExpr JSExpr
  | Lt JSExpr JSExpr
  | Ge JSExpr JSExpr
  | Le JSExpr JSExpr
  | Not JSExpr
  | And JSExpr JSExpr
  | Or JSExpr JSExpr
  | Eq JSExpr JSExpr
  deriving (Show)

data JSStatement
  = Const T.Text JSExpr
  | Let T.Text JSExpr
  | Set T.Text JSExpr
  | If JSExpr [JSStatement]
  | IfElse JSExpr [JSStatement] [JSStatement]
  | WhileLoop JSExpr [JSStatement]
  | Return JSExpr
  | ExprStatement JSExpr
  | Reset [T.Text] [JSExpr] -- to be able to write [a, b, c] = [1, 2, 3]; for TCO
  | Continue
  deriving (Show)

factorialInJSWithTCO' :: JSStatement
factorialInJSWithTCO' =
  Const
    "fac"
    ( FExpr
        ["n"]
        [ Return $
            FCall
              ( FExpr
                  []
                  [ Const
                      "fac_tailcall"
                      ( FExpr
                          ["x", "acc"]
                          [ WhileLoop
                              (JSBoolean True)
                              [ If (Eq (Var "x") (JSNumber 1)) [Return (Var "acc")],
                                Reset ["x", "acc"] [Sub (Var "x") (JSNumber 1), Mul (Var "acc") (Var "x")]
                              ]
                          ]
                      ),
                    Return (FCall (Var "fac_tailcall") [Var "n", JSNumber 1])
                  ]
              )
              []
        ]
    )

factorialInJSWithWhileLoop :: JSStatement
factorialInJSWithWhileLoop =
  Const
    "factorial"
    ( FExpr
        ["n"]
        [ Let "result" (JSNumber 1),
          WhileLoop
            (Gt (Var "n") (JSNumber 1))
            [ Set "result" (Mul (Var "result") (Var "n")),
              Set "n" (Sub (Var "n") (JSNumber 1))
            ],
          Return (Var "result")
        ]
    )

factorialInJS :: JSStatement
factorialInJS =
  Const "factorial" $
    FExpr
      ["n"]
      [ IfElse
          (Eq (Var "n") (JSNumber 1))
          [Return (JSNumber 1)]
          [Return (Mul (Var "n") (FCall (Var "fibo") [Sub (Var "n") (JSNumber 1)]))]
      ]

fiboIfElse :: JSStatement
fiboIfElse =
  IfElse
    (Eq (Var "n") (JSNumber 1))
    [Return (JSNumber 1)]
    [Return (Mul (Var "n") (FCall (Var "fibo") [Sub (Var "n") (JSNumber 1)]))]

type Output = StateT Int (Writer T.Text) ()

class JSPrint a where
  jsPrint :: a -> Output

prec :: (Num a) => JSExpr -> a
prec (FCall {}) = 16
prec (JSArray {}) = 16
prec (JSNumber {}) = 16
prec (JSString {}) = 16
prec (JSBoolean {}) = 16
prec (JSObject {}) = 16
prec (Var {}) = 16
prec (FExpr {}) = 15
prec (Not {}) = 14
prec (Mul {}) = 12
prec (Div {}) = 12
prec (Add {}) = 11
prec (Sub {}) = 11
prec (Gt {}) = 9
prec (Lt {}) = 9
prec (Ge {}) = 9
prec (Le {}) = 9
prec (And {}) = 4
prec (Eq {}) = 8
prec (Or {}) = 3
prec (Ternary {}) = 2

wrapIfHigher :: JSExpr -> JSExpr -> Output
wrapIfHigher e1 e2 =
  if prec e1 <= prec e2
    then jsPrint e2
    else tell "(" *> jsPrint e2 *> tell ")"

intercalateW :: Output -> [Output] -> Output
intercalateW _ [] = pure ()
intercalateW _ [x] = x
intercalateW sep (x : xs) = x *> sep *> intercalateW sep xs

printObjectContents :: [(T.Text, JSExpr)] -> Output
printObjectContents = intercalateW (tell ", ") . map (\(key, value) -> tell (key <> ": ") *> jsPrint value)

block :: [JSStatement] -> Output
block s = tell "{\n" *> modify (+ 2) *> traverse_ jsPrint s *> modify (+ (-2)) *> indent (tell "}")

instance (JSPrint a) => JSPrint [a] where
  jsPrint a = intercalateW (tell "\n") (jsPrint <$> a)

instance JSPrint JSExpr where
  jsPrint (JSNumber n) = tell . T.pack $ show n
  jsPrint (JSString s) = tell . T.pack $ show s
  jsPrint (Var s) = tell s
  jsPrint (JSBoolean v) = tell $ if v then "true" else "false"
  jsPrint (JSObject o) = tell "{ " *> printObjectContents o *> tell " }"
  jsPrint op@(Ternary cond ifT ifF) = wrap cond *> tell " ? " *> wrap ifT *> tell " : " *> wrap ifF
    where
      wrap = wrapIfHigher op
  jsPrint (FExpr args body) = tell "(" *> intercalateW (tell ", ") (map tell args) *> tell ") => " *> block body
  jsPrint (JSArray a) = tell "[" *> intercalateW (tell ", ") (jsPrint <$> a) *> tell "]"
  jsPrint c@(FCall exp args) = wrapIfHigher c exp *> tell "(" *> intercalateW (tell ", ") (map jsPrint args) *> tell ")"
  jsPrint m@(Mul x y) = wrap x *> tell " * " *> wrap y
    where
      wrap = wrapIfHigher m
  jsPrint a@(Add x y) = wrap x *> tell " + " *> wrap y
    where
      wrap = wrapIfHigher a
  jsPrint s@(Sub x y) = wrap x *> tell " - " *> wrap y
    where
      wrap = wrapIfHigher s
  jsPrint d@(Div x y) = wrap x *> tell " / " *> wrap y
    where
      wrap = wrapIfHigher d
  jsPrint op@(Not x) = let wrap = wrapIfHigher op in tell "!" *> wrap x
  jsPrint op@(Gt x y) = let wrap = wrapIfHigher op in wrap x *> tell " > " *> wrap y
  jsPrint op@(Lt x y) = let wrap = wrapIfHigher op in wrap x *> tell " < " *> wrap y
  jsPrint op@(Ge x y) = let wrap = wrapIfHigher op in wrap x *> tell " >= " *> wrap y
  jsPrint op@(Le x y) = let wrap = wrapIfHigher op in wrap x *> tell " <= " *> wrap y
  jsPrint op@(And x y) = let wrap = wrapIfHigher op in wrap x *> tell " && " *> wrap y
  jsPrint op@(Or x y) = let wrap = wrapIfHigher op in wrap x *> tell " || " *> wrap y
  jsPrint op@(Eq x y) = let wrap = wrapIfHigher op in wrap x *> tell " === " *> wrap y

indent :: Output -> Output
indent stm = (get >>= \ident -> tell $ T.replicate ident " ") *> stm

statement :: Output -> Output
statement stm = indent stm *> tell ";\n"

instance JSPrint JSStatement where
  jsPrint (Const name e) = statement $ tell "const " *> tell name *> tell " = " *> jsPrint e
  jsPrint (Let name e) = statement $ tell "let " *> tell name *> tell " = " *> jsPrint e
  jsPrint (Set name e) = statement $ tell name *> tell " = " *> jsPrint e
  jsPrint (If e s) = indent (tell "if (") *> jsPrint e *> tell ") " *> block s *> tell "\n"
  jsPrint (IfElse e ifT ifF) = indent (tell "if (") *> jsPrint e *> tell ") " *> block ifT *> tell " else " *> block ifF *> tell "\n"
  jsPrint (WhileLoop exp stmnts) = indent (tell "while (") *> jsPrint exp *> tell ") " *> block stmnts *> tell "\n"
  jsPrint (Return e) = statement $ tell "return " *> jsPrint e
  jsPrint (ExprStatement e) = statement $ jsPrint e
  jsPrint (Reset names values) =
    statement $ do
      tell "["
      intercalateW (tell ", ") (map tell names)
      tell "] = ["
      intercalateW (tell ", ") (map jsPrint values)
      tell "]"
  jsPrint Continue = statement $ tell "continue"

printJS :: (JSPrint a) => a -> IO ()
printJS x = TIO.putStrLn . snd . runWriter $ runStateT (jsPrint x) 0