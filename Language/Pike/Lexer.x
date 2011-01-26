{
module Language.Pike.Lexer where

import Language.Pike.Tokens
import Data.ByteString.Lazy.Char8 as LBS
import Language.Pike.CompileError
import Control.Monad.State.Strict
import Control.Monad.Error

}

tokens :-
  $white+                 ;
  array                   { key KeyArray }
  bool                    { key KeyBool }
  break                   { key KeyBreak }
  class                   { key KeyClass }
  else                    { key KeyElse }
  for                     { key KeyFor }
  if                      { key KeyIf }
  import                  { key KeyImport }
  int                     { key KeyInt }
  lambda                  { key KeyLambda }
  public                  { key KeyPublic }
  return                  { key KeyReturn }
  string                  { key KeyString }
  void                    { key KeyVoid }
  while                   { key KeyWhile }
  ":"                     { \p l -> const Colon }
  ";"                     { \p l -> const Semicolon }
  "."                     { \p l -> const Dot }
  ","                     { \p l -> const Comma }
  "("                     { \p l -> const (Bracket Parenthesis False) }
  ")"                     { \p l -> const (Bracket Parenthesis True) }
  "["                     { \p l -> const (Bracket Square False) }
  "]"                     { \p l -> const (Bracket Square True) }
  "{"                     { \p l -> const (Bracket Curly False) }
  "}"                     { \p l -> const (Bracket Curly True) }
  "=="                    { op OpEqual }
  "="                     { op OpAssign }
  "->"                    { op OpAccess }
  "<="                    { op OpLessEq }
  "<"                     { op OpLess }
  [a-zA-Z] [a-zA-Z0-9\_]* { \p s l -> Identifier (LBS.unpack $ LBS.take (fromIntegral l) s) }
  "-"? [0-9]+             { \p s l -> ConstInt $ read (LBS.unpack $ LBS.take (fromIntegral l) s) }
  \" (. # \")* \"         { \p s l -> ConstString $ read (LBS.unpack $ LBS.take (fromIntegral l) s) }
  "+"                     { op OpPlus }
  "-"                     { op OpMinus }
{

data LexerPos = LexerPos !Int !Int deriving Show

data LexerState = LexerState
                  { pos :: !LexerPos
                  , input :: LBS.ByteString
                  , curChar :: !Char
                  , curCode :: !Int
                  } deriving Show

type Lexer a = ErrorT (CompileError (Int,Int)) (State LexerState) a

type AlexInput = (LexerPos,Char,LBS.ByteString)

lexerStartPos :: LexerPos
lexerStartPos = LexerPos 1 0

runLexer :: Lexer a -> LBS.ByteString -> Either (CompileError (Int,Int)) a
runLexer lex bs = evalState (runErrorT lex) (LexerState lexerStartPos bs '\n' 0)

lexerMove :: LexerPos -> Char -> LexerPos
lexerMove (LexerPos l c) '\t' = LexerPos l (((c+7) `div` 8)*8+1)
lexerMove (LexerPos l c) '\n' = LexerPos (l+1) 1
lexerMove (LexerPos l c) _    = LexerPos l (c+1)

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (pos,c,inp)
  | LBS.null inp = Nothing
  | otherwise = let c  = LBS.head inp
                    cs = LBS.tail inp
                    p  = lexerMove pos c
                in p `seq` cs `seq` Just (c, (p,c,cs))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

scanToken :: Lexer Token
scanToken = do
  st <- get
  case alexScan (pos st,curChar st,input st) (curCode st) of
    AlexEOF -> return EOF
    AlexError (LexerPos l c,curC,rest) -> throwError (LexicalError (LBS.head rest) l c)
    AlexSkip (npos,curC,rest) len -> do
      put (st { pos = npos
              , input = rest
              , curChar = curC
              })
      scanToken
    AlexToken (npos,curC,rest) len act -> do
      put (st { pos = npos
              , input = rest
              , curChar = curC
              })
      return $ act (pos st) (input st) len

pikeLexer :: (Token -> Lexer a) -> Lexer a
pikeLexer f = do
  tok <- scanToken
  f tok

getPos :: Lexer (Int,Int)
getPos = do
  LexerState { pos = LexerPos l c } <- get
  return (l,c)

key :: Keyword -> LexerPos -> LBS.ByteString -> Int -> Token
key w _ _ _ = Key w

op :: Operator -> LexerPos -> LBS.ByteString -> Int -> Token
op w _ _ _ = Op w
}