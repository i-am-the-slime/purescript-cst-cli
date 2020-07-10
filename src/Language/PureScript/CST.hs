module Language.PureScript.CST
  ( parseFromFile
  , parseModuleFromFile
  , unwrapParserError
  , pureResult
  , module Language.PureScript.CST.Convert
  , module Language.PureScript.CST.Lexer
  , module Language.PureScript.CST.Monad
  , module Language.PureScript.CST.Parser
  , module Language.PureScript.CST.Print
  , module Language.PureScript.CST.Types
  ) where

import Prelude hiding (lex)

import Control.Monad.Error.Class (MonadError(..))
import Control.Parallel.Strategies (withStrategy, parList, evalTuple2, r0, rseq)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Language.PureScript.AST as AST
import Language.PureScript.CST.Convert
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Lexer
import Language.PureScript.CST.Monad (Parser, ParserM(..), ParserState(..), LexResult, runParser, runTokenParser)
import Language.PureScript.CST.Parser
import Language.PureScript.CST.Print
import Language.PureScript.CST.Types

pureResult :: a -> PartialResult a
pureResult a = PartialResult a ([], pure a)

parseModuleFromFile :: FilePath -> Text -> Either (NE.NonEmpty ParserError) (PartialResult AST.Module)
parseModuleFromFile fp content = fmap (convertModule fp) <$> parseModule (lex content)

parseFromFile :: FilePath -> Text -> ([ParserWarning], Either (NE.NonEmpty ParserError) AST.Module)
parseFromFile fp content = fmap (convertModule fp) <$> parse content

handleParserError
  :: forall m k a
   . MonadError Text m
  => (k -> FilePath)
  -> (k, Either (NE.NonEmpty ParserError) a)
  -> m (k, a)
handleParserError toFilePath (k, res) =
  (k,) <$> unwrapParserError (toFilePath k) res

unwrapParserError
  :: forall m a
   . MonadError Text m
  => FilePath
  -> Either (NE.NonEmpty ParserError) a
  -> m a
unwrapParserError fp =
  either (throwError . fail fp) pure
