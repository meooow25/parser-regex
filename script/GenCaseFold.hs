#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, parser-regex, text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative ((<|>), many)
import Control.Monad (replicateM, when)
import System.Environment (getArgs)
import System.Exit (die)
import Data.Char (chr, ord)
import qualified Data.Foldable as F
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Numeric as Num
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as T
import qualified Regex.Text as R

main :: IO ()
main = do
  args <- getArgs
  case args of
    [caseFoldingTxtIn, hsOutFile, hsOutModuleName] ->
      run caseFoldingTxtIn hsOutFile hsOutModuleName
    _ -> die "Usage: cabal run script -- CASE_FOLDING_TXT_IN HS_OUT_DIR HS_OUT_MODULE_NAME"

run :: FilePath -> FilePath -> String -> IO ()
run inf outf outMod = do
  intxt <- T.readFile inf
  case R.reParse caseFoldingRe intxt of
    Nothing -> die $ "Parsing " ++ inf ++ " failed"
    Just cf -> case mkHs outMod cf of
      Left e -> die $ "Generating hs failed: " ++ e
      Right t -> do
        T.writeFile outf t
        putStrLn $ "Done! Generated " ++ outf

-- See Note [Why simple case fold] in Regex.Internal.Text
mkHs :: String -> CaseFolding -> Either String Text
mkHs outMod (CaseFolding h cs) = do
  let fwd = M.fromList cs'
  when (M.size fwd /= length cs') $
    Left "fwd: duplicate source Char"
  pure $ outHs fwd
  where
    cs' = flip mapMaybe cs $ \case
      Common c d -> Just (c,d)
      Simple c d -> Just (c,d)
      Full _ _   -> Nothing
      Turkic _ _ -> Nothing
    outHs fwd = T.unlines $
      [ "-- DO NOT EDIT"
      , "-- This file was generated by GenCaseFold.hs from a CaseFolding.txt with header"
      , "--"
      ] ++
      map ("-- " <>) h ++
      [ "--"
      , "module " <> T.pack outMod
      , "  ( caseFoldSimple"
      , "  ) where"
      , ""
      , "caseFoldSimple :: Char -> Char"
      , "caseFoldSimple c0 = case c0 of"
      ] ++
      [ "  " <> hexShowChar c <> " -> " <> hexShowChar d
      | (c,d) <- M.assocs fwd
      ] ++
      [ "  c -> c"
      , "{-# NOINLINE caseFoldSimple #-}"
      ]

data CaseFolding = CaseFolding
   ![Text] -- header lines
   ![CaseFold]

data CaseFold
  = Common !Char !Char -- from to
  | Simple !Char !Char
  | Full !Char ![Char]
  | Turkic !Char !Char
  deriving Show

caseFoldingRe :: R.REText CaseFolding
caseFoldingRe =
  CaseFolding
    <$> header
    <* commentOrBlank `R.endBy` newl
    <*> one `R.endBy` newl
    <* commentOrBlank `R.endBy` newl
  where
    comment = R.text "# " *> R.manyTextMin
    commentOrBlank = comment <|> R.text "#" <|> pure ""
    newl = R.char '\n'
    header = replicateM 3 (comment <* newl)
    hexCode = many (R.char '0') *>
              fmap (chr . fromIntegral)
                   (R.wordRangeHex (0, fromIntegral (ord (maxBound :: Char))))
    manyHexCode = hexCode `R.sepBy1` R.char ' '
    one = F.asum
      [ Common <$> hexCode <* R.text "; C; " <*> hexCode <* R.text "; " <* comment
      , Simple <$> hexCode <* R.text "; S; " <*> hexCode <* R.text "; " <* comment
      , Full <$> hexCode <* R.text "; F; " <*> manyHexCode <* R.text "; " <* comment
      , Turkic <$> hexCode <* R.text "; T; " <*> hexCode <* R.text "; " <* comment
      ]

hexShowChar :: Char -> Text
hexShowChar c = T.pack ("'\\x" ++ Num.showHex (ord c) "'")
