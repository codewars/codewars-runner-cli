{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleInstances #-}
module Test.CodeWars.BlackList (Hidden (..), BlackList, hidden) where
import Language.Haskell.Exts.Annotated (parseFile, ParseResult (ParseOk, ParseFailed), SrcLoc(..) )
import qualified Language.Haskell.Exts.Syntax as Syntax (Module (..))
import Language.Haskell.Exts.Syntax (ImportDecl (..), Module (), ModuleName (..), ImportSpec (..), Name (..), CName (..))
import Language.Haskell.Exts.Annotated.Simplify (sModule)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Text.Printf (printf)
import Control.Monad (join)
-- TODO: Switch this massive hack to use an environment variable
import System.Environment (getEnv)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit (assertBool)
import Data.List (intercalate)

data Hidden
  -- | Hide an entire module
  = Module String
  -- | Hide a symbol in a module
  | FromModule {moduleName :: String,
                symbolName :: String}

instance Show Hidden where
  show (Module moduleName) = moduleName
  show (FromModule {..}) = join [moduleName, ".", symbolName]

-- | Parse the solution code
solutionModule :: IO Module
solutionModule = do
  getEnv "solutionFileName"
  >>= parseFile
  >>= \case
    ParseOk annotatedModule ->
      return $ sModule annotatedModule
    ParseFailed (SrcLoc {..}) msg ->
      fail $ printf
        "Could not parse file %s (line %d, column %d): %s"
        srcFilename srcLine srcColumn msg

-- | Extract the import declarations from the solution
solutionImportDeclarations :: IO [ImportDecl]
solutionImportDeclarations = do
  (Syntax.Module _ _ _ _ _ imports _) <- solutionModule
  return imports

-- | A class for specifying that something is to be hidden in the solution imports
class BlackList a where
  hidden :: a -> Expectation

instance BlackList Hidden where
  hidden h = do
    decls <- solutionImportDeclarations
    let declCode = intercalate "\n" $ map prettyPrint decls
    let mustHideMessage = printf "Import declarations must hide %s" (show h)
    let errorMessage = if (declCode /= "")
                       then join [mustHideMessage, ":\n\n", declCode]
                       else mustHideMessage
    assertBool errorMessage $ importDeclarationsHide decls h

instance BlackList [Hidden] where
  hidden = mapM_ hidden

-- | Check that an import declaration hides a particular module or symbol
importDeclarationsHide :: [ImportDecl] -> Hidden -> Bool
importDeclarationsHide decls preludeSymbol@(FromModule "Prelude" symbolName) =
  not (any (`importDeclImports` preludeSymbol) decls) && any hidesSymbol decls
  where
    hidesSymbol
      (ImportDecl {importModule = ModuleName "Prelude",
                   importSpecs = Just (True, importSpecs)}) = any (`importSpecMatchesSymbol` symbolName) importSpecs
    hidesSymbol _ = False
importDeclarationsHide decls hidden = not $ any (`importDeclImports` hidden) decls

-- | Check that an import declaration imports a particular module or symbol
importDeclImports :: ImportDecl -> Hidden -> Bool
importDeclImports
  (ImportDecl {importModule = ModuleName importedModuleName})
  (Module moduleName) = moduleName == importedModuleName

importDeclImports
  (ImportDecl { importModule = ModuleName importedModuleName
              , importSpecs = Nothing })
  (FromModule {..}) = moduleName == importedModuleName
importDeclImports
  (ImportDecl {importModule = ModuleName importedModuleName,
               importSpecs = Just (isHidden, importSpecs)})
  (FromModule {..}) =
    moduleName == importedModuleName &&
    (any (`importSpecMatchesSymbol` symbolName) importSpecs) /= isHidden

-- | Deconstruct a Named string from Language.Haskell.Exts.Syntax.Name
unName :: Name -> String
unName (Ident name) = name
unName (Symbol name) = name

-- | Check that an ImportSpec matches a symbol
importSpecMatchesSymbol :: ImportSpec -> String -> Bool
(IVar x) `importSpecMatchesSymbol` y = (unName x) == y
(IAbs _ x) `importSpecMatchesSymbol` y = (unName x) == y
(IThingAll x) `importSpecMatchesSymbol` y = (unName x) == y
(IThingWith x cnames) `importSpecMatchesSymbol` y =
  (unName x) == y || cnames `anyConstructorMatchesSymbol` y

-- | Check that some constructors in a list of constructors matches a symbol
anyConstructorMatchesSymbol :: [CName] -> String -> Bool
anyConstructorMatchesSymbol cnames y = any matches cnames
  where
    matches (VarName x) = (unName x) == y
    matches (ConName x) = (unName x) == y
