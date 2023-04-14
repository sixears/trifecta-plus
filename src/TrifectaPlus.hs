{-| Like `Parsecable`, but using `CharParsing`. -}
module TrifectaPlus
  ( eiText, liftTParse, liftTParse', tParse, tParse', tParseFile, testParse
  , testParse', testParseE, tname )
where

import Base1T

-- bytestring --------------------------

import Data.ByteString  ( ByteString )

-- fpath -------------------------------

import FPath.AsFilePath  ( filepath )
import FPath.File        ( FileAs )

-- parsers -----------------------------

import Text.Parser.Combinators  ( eof )

-- prettyprinter -----------------------

import Prettyprinter.Render.Text  ( renderStrict )
import Prettyprinter.Internal     ( defaultLayoutOptions, layoutPretty )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, assertFailure )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( Lift )

-- text --------------------------------

import Data.Text  ( isInfixOf, length, replace, take, unpack )

-- textual-plus -------------------

import TextualPlus'  ( TextualPlus( textual' ) )

-- trifecta ----------------------------

import Text.Trifecta.Parser  ( Parser
                             , parseFromFileEx, parseByteString, parseString )
import Text.Trifecta.Result  ( AsResult, ErrInfo, Result( Failure, Success )
                             , _Success, _errDoc )

--------------------------------------------------------------------------------

class TParse ρ where
  parseT ∷ ∀ α . Parser α → ρ → Result α

--------------------

instance TParse 𝕊 where
  parseT p = parseString p ф

--------------------

instance TParse 𝕋 where
  parseT p = parseT p ∘ unpack

--------------------

instance TParse ByteString where
  parseT p = parseByteString p ф

------------------------------------------------------------

{-| parse a value using trifecta -}
tParse  ∷ (TextualPlus α, TParse ρ) ⇒ ρ → Result α
tParse  = parseT textual'

{-| like `tParse`, but parse *precisely* the text given - i.e., with an `eof` -}
tParse'  ∷ (TextualPlus α, TParse ρ) ⇒ ρ → Result α
tParse'  = parseT (textual' ⋪ eof)

----------------------------------------

tParseFile ∷ ∀ α μ ρ . (MonadIO μ, TextualPlus α, FileAs ρ) ⇒ ρ → μ (Result α)
tParseFile f = parseFromFileEx textual' (f ⫥ filepath)

----------------------------------------

{-| Parse a file, line-by-line.
    unused at this stage, this is just for recording line-based parsing. -}
-- tParseFileLines ∷ (MonadIO μ, TextualPlus α, FileAs ρ) ⇒ ρ → μ (Result α)
-- tParseFileLines f = parseFromFileEx (runUnlined textual') (f ⫥ filepath)

----------------------------------------

tname ∷ 𝕋 → 𝕊
tname "" = "«empty»"
tname t = let t' = replace "\t" "\\t" $ replace "\r" "\\r" $ replace "\n" "\\n"t
           in unpack $ if 32 < length t'
                       then take 31 t' ⊕ "…"
                       else t'

{-| Test that some text parses to a given value. -}
-- HasCallStack ensures that the test failure is cited from the callee
testParse' ∷ (Eq α, Show α, HasCallStack) ⇒ (𝕋 → Result α) → 𝕋 → α → TestTree
testParse' parser input expect =
  testCase (tname input) $
--    𝕵 expect @=? (parser input) ⩼ _Success
    case parser input of
      Success x → expect @=? x
      Failure e → assertFailure ∘ unpack $ eiText e

{-| Test that some text parses to a given value; using `tParse'`. -}
-- HasCallStack ensures that the test failure is cited from the callee
testParse ∷ (Eq α, Show α, TextualPlus α, HasCallStack) ⇒ 𝕋 → α → TestTree
testParse = testParse' tParse'

----------------------------------------

{-| Extract error text from an ErrInfo. -}
eiText ∷ ErrInfo → 𝕋
-- layoutCompact splits the message string into separate lines
eiText = renderStrict ∘ layoutPretty defaultLayoutOptions ∘ _errDoc

----------------------------------------

{-| Check that an attempted parse results in an error **containing**
    (`isInfixOf`) the given text.

    This needs to take a "parser" giving a `Result`, to specify the type (@α@)
    for the parse; without that, the parse type is ambiguous.  Typically, that's
    an instantiation of @parseT@ with an appropriate TypeApplication.

    If @exp@ is @""@, then a simple outright parse failure (returning @Nothing@)
    will pass the test; as will any failure.  Otherwise, an actual failure is
    required, whose text contains @exp@.
-}
-- HasCallStack ensures that the test failure is cited from the callee
testParseE ∷ (Show α, HasCallStack) ⇒ 𝕋 → (𝕋 → Result α) → 𝕋 → TestTree
testParseE input prse exp =
  testCase (tname input) $
    let result = prse input
    in  case result of
          Success a → if "" ≡ exp
                      then assertFailure $ [fmt|expected an error, got %w|] a
                      else assertFailure $
                             [fmt|expected error '%t' got «%w»|] exp a
          Failure e → if "" ≡ exp
                      then assertSuccess $ [fmt|parse failed (%t)|] input
                      else let v = eiText e
                           in  assertBool (unpack v) $ exp `isInfixOf` v

----------------------------------------

{-| Lift a `tParse` or similar to use in a splice, e.g., for quasiquoting. -}
liftTParse ∷ ∀ χ τ α . (Lift χ, AsResult α α χ χ) ⇒ (τ → α) → τ → 𝕄 ExpQ
liftTParse f = (\ x → ⟦x⟧) ⩺ ((⩼ _Success) ∘ f)

{-| Simpler type sig for `liftTParse`. -}
liftTParse' ∷ ∀ α . Lift α ⇒ (𝕊 → Result α) → 𝕊 → 𝕄 ExpQ
liftTParse' = liftTParse

-- that's all, folks! ----------------------------------------------------------
