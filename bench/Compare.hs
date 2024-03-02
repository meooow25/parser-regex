{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Compare (benches) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Char
import Data.Array
import qualified Data.Foldable as F
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import GHC.Generics (Generic)

import Test.Tasty (testGroup)
import Test.Tasty.Bench
import Test.Tasty.HUnit (testCase, (@?=))

-- parser-regex
import qualified Regex.Text as RT
import qualified Regex.List as RL
import qualified Data.CharSet as CS

-- regex-applicative
import qualified Text.Regex.Applicative as RA
import qualified Text.Regex.Applicative.Common as RA

-- regex-pcre-builtin
import qualified Text.Regex.PCRE.ByteString as PCREBS

-- regex-tdfa
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.Base as RBase

-- regex
import qualified Text.RE.Replace as Replace
import qualified Text.RE.TDFA.Text as TDFAReplace

-- regex-with-pcre
import qualified Text.RE.PCRE.ByteString as PCREReplace

benches :: Benchmark
benches = bgroup "compare"
  [ env englishText $ \ ~(t,b,s) ->
    bgroup "English text 1"
    [ bench "parser-regex T" $ nf english1PR t
    , bench "parser-regex S" $ nf english1PRS s
    , bench "regex-applicative S" $ nf english1RA s
    , bench "regex-tdfa T" $ nf english1TDFA t
    , bench "regex-pcre-builtin BS" $ nf english1PCRE b
    , testGroup "tests"
      [ testCase "check count" $ length (english1PR t) @?= 900
      , testCase "S == L" $ map T.pack (english1PRS s) @?= english1PR t
      , testCase "regex-applicative ==" $ map T.pack (english1RA s) @?= english1PR t
      , testCase "regex-tdfa ==" $ english1TDFA t @?= english1PR t
      , testCase "regex-pcre-builtin ==" $ map TEnc.decodeUtf8 (english1PCRE b) @?= english1PR t
      ]
    ]
  , env englishText $ \ ~(t,b,s) ->
    bgroup "English text 2"
    [ bench "parser-regex T" $ nf english2PR t
    , bench "parser-regex S" $ nf english2PRS s
    , bench "regex-applicative S" $ nf english2RA s
    , bench "regex-tdfa T" $ nf english2TDFA t
    , bench "regex-pcre-builtin BS" $ nf english2PCRE b
    , testGroup "tests"
      [ testCase "check count" $ length (english2PR t) @?= 365
      , testCase "S == T" $ map T.pack (english2PRS s) @?= english2PR t
      , testCase "regex-applicative ==" $ map T.pack (english2RA s) @?= english2PR t
      , testCase "regex-tdfa ==" $ english2TDFA t @?= english2PR t
      , testCase "regex-pcre-builtin ==" $ length (english2PCRE b) @?= 354
        -- pcre count doesn't match because it matches bytes and not Chars
      ]
    ]
  , env englishText $ \ ~(t,b,s) ->
    bgroup "English replace all"
    [ bench "parser-regex T" $ nf englishReplacePR t
    , bench "parser-regex S" $ nf englishReplacePRS s
    , bench "regex-applicative S" $ nf englishReplaceRA s
    , bench "regex-tdfa T" $ nf englishReplaceTDFA t
    , bench "regex-pcre-builtin BS" $ nf englishReplacePCRE b
    , testGroup "tests"
      [ testCase "S == T" $ T.pack (englishReplacePRS s) @?= englishReplacePR t
      , testCase "regex-applicative ==" $ T.pack (englishReplaceRA s) @?= englishReplacePR t
      , testCase "regex-tdfa ==" $ englishReplaceTDFA t @?= englishReplacePR t
      , testCase "regex-pcre-builtin ==" $ englishReplacePCRE b @?= TEnc.encodeUtf8 (englishReplacePR t)
      ]
    ]
  , env caseFoldingTxt $ \ ~(t,b,s) ->
    bgroup "Parse CaseFolding.txt"
    [ bench "parser-regex T" $ nf caseFoldingPR t
    , bench "parser-regex S" $ nf caseFoldingPRS s
    , bench "regex-applicative S" $ nf caseFoldingRA s
    , bench "regex-tdfa T" $ nf caseFoldingTDFA t
    , bench "regex-pcre-builtin BS" $ nf caseFoldingPCRE b
    , testGroup "tests"
      [ testCase "check count" $ length (caseFoldingPR t) @?= 1563
      , testCase "S == T" $ caseFoldingPRS s @?= caseFoldingPR t
      , testCase "regex-applicative ==" $ caseFoldingRA s @?= caseFoldingPR t
      , testCase "regex-tdfa ==" $ caseFoldingTDFA t @?= caseFoldingPR t
      , testCase "regex-pcre-builtin ==" $ caseFoldingPCRE b @?= caseFoldingPR t
      ]
    ]
  , env htmlText $ \ ~(t,b,s) ->
    bgroup "Parse URI"
    [ bench "parser-regex T" $ nf uriPR t
    , bench "parser-regex S" $ nf uriPRS s
    , bench "regex-applicative S" $ nf uriRA s
    , bench "regex-tdfa T" $ nf uriTDFA t
    , bench "regex-pcre-builtin BS" $ nf uriPCRE b
    , testGroup "tests"
      [ testCase "check count" $ length (uriPR t) @?= 4277
      , testCase "S == T" $ map uriS2T (uriPRS s) @?= uriPR t
      , testCase "regex-applicative ==" $ map uriS2T (uriRA s) @?= uriPR t
      , testCase "regex-tdfa ==" $ uriTDFA t @?= uriPR t
      , testCase "regex-pcre-builtin ==" $ uriPCRE b @?= map uriT2BS (uriPR t)
      ]
    ]
  , bgroup "Exponential backtracking"
    [ bench "parser-regex T" $ whnf expPR expText
    , bench "parser-regex S" $ whnf expPRS expString
    , bench "regex-applicative S" $ whnf expRA expString
    , bench "regex-tdfa T" $ whnf expTDFA expText
    , bench "regex-pcre-builtin BS" $ whnf expPCRE expBS
    , testGroup "tests"
      [ testCase "parser-regex T True" $ expPR expText @?= True
      , testCase "parser-regex S True" $ expPRS expString @?= True
      , testCase "regex-applicative True" $ expRA expString @?= True
      , testCase "regex-tdfa True" $ expTDFA expText @?= True
      , testCase "regex-pcre-builtin True" $ expPCRE expBS @?= True
      ]
    ]
  ]

-------------------
-- English text 1
-------------------

-- "Tom|Sawyer|Huckleberry|Finn"
-- From https://rust-leipzig.github.io/regex/2017/03/28/comparison-of-regex-engines/

englishText :: IO (Text, ByteString, String)
englishText = readBenchData "74.txt.utf-8"

-- parser-regex
english1PR :: Text -> [Text]
english1PR = RT.findAll re
  where
    re = F.asum $ map RT.text ["Tom","Sawyer","Huckleberry","Finn"]

english1PRS :: String -> [String]
english1PRS = RL.findAll re
  where
    re = F.asum $ map RL.list ["Tom","Sawyer","Huckleberry","Finn"]

-- regex-applicative
english1RA :: String -> [String]
english1RA = fromJust . RA.match re
  where
    re = toFindManyRA $
         F.asum $ map RA.string ["Tom","Sawyer","Huckleberry","Finn"]

-- regex-tdfa
english1TDFA :: Text -> [Text]
english1TDFA = map (fst . (! 0)) . RBase.matchAllText re
  where
    re :: TDFA.Regex
    re = RBase.makeRegexOpts RBase.blankCompOpt RBase.blankExecOpt
         ("Tom|Sawyer|Huckleberry|Finn" :: Text)

-- regex-pcre-builtin
english1PCRE :: ByteString -> [ByteString]
english1PCRE = map (fst . (! 0)) . RBase.matchAllText re
  where
    re :: PCREBS.Regex
    re = RBase.makeRegexOpts RBase.blankCompOpt RBase.blankExecOpt
         ("Tom|Sawyer|Huckleberry|Finn" :: ByteString)

-------------------
-- English text 2
-------------------

-- "“[^?!.]{0,30}[?!.]”"
-- Adapted from https://rust-leipzig.github.io/regex/2017/03/28/comparison-of-regex-engines/

-- parser-regex
english2PR :: Text -> [Text]
english2PR = RT.findAll re
  where
    re = RT.toMatch $
           RT.char '“' *>
           RT.betweenCount (0,30) (RT.oneOf (CS.not "?!.")) *>
           RT.oneOf "?!." *>
           RT.char '”'

english2PRS :: String -> [String]
english2PRS = RL.findAll re
  where
    re = RL.toMatch $
           RL.single '“' *>
           RL.betweenCount (0,30) (RL.oneOfChar (CS.not "?!.")) *>
           RL.oneOfChar "?!." *>
           RL.single '”'

-- regex-applicative
english2RA :: String -> [String]
english2RA = fromJust . RA.match re
  where
    re = toFindManyRA $
         (fmap snd . RA.withMatched) $
           RA.sym '“' *>
           betweenCountRA (0,30) (RA.psym (`notElem` ("?!." :: String))) *>
           RA.psym (\c -> c `elem` ("?!." :: String)) *>
           RA.sym '”'

-- regex-tdfa
english2TDFA :: Text -> [Text]
english2TDFA = map (fst . (! 0)) . RBase.matchAllText re
  where
    re :: TDFA.Regex
    re = RBase.makeRegexOpts RBase.blankCompOpt RBase.blankExecOpt
         ("“[^?!.]{0,30}[?!.]”" :: Text)

-- regex-pcre-builtin
english2PCRE :: ByteString -> [ByteString]
english2PCRE = map (fst . (! 0)) . RBase.matchAllText re
  where
    re :: PCREBS.Regex
    re = RBase.makeRegexOpts PCREBS.compDotAll RBase.blankExecOpt
         (TEnc.encodeUtf8 "“[^?!.]{0,30}[?!.]”")

--------------------
-- English replace
--------------------

-- parser-regex
englishReplacePR :: Text -> Text
englishReplacePR = RT.replaceAll $
      "Huckleberry" <$ RT.text "Tom"
  <|> "Finn"        <$ RT.text "Sawyer"
  <|> "Tom"         <$ RT.text "Huckleberry"
  <|> "Sawyer"      <$ RT.text "Finn"

englishReplacePRS :: String -> String
englishReplacePRS = RL.replaceAll $
      "Huckleberry" <$ RL.list "Tom"
  <|> "Finn"        <$ RL.list "Sawyer"
  <|> "Tom"         <$ RL.list "Huckleberry"
  <|> "Sawyer"      <$ RL.list "Finn"

-- regex-applicative
englishReplaceRA :: String -> String
englishReplaceRA = RA.replace $
      "Huckleberry" <$ RA.string "Tom"
  <|> "Finn"        <$ RA.string "Sawyer"
  <|> "Tom"         <$ RA.string "Huckleberry"
  <|> "Sawyer"      <$ RA.string "Finn"

-- regex
englishReplaceTDFA :: Text -> Text
englishReplaceTDFA t =
  Replace.replaceAllCaptures Replace.TOP repl $ t TDFAReplace.*=~ re
  where
    re = fromJust $ TDFAReplace.compileRegex "Tom|Sawyer|Huckleberry|Finn"
    repl _ _ cap = case Replace.capturedText cap of
      "Tom"         -> Just "Huckleberry"
      "Sawyer"      -> Just "Finn"
      "Huckleberry" -> Just "Tom"
      "Finn"        -> Just "Sawyer"
      _             -> error "impossible"

englishReplacePCRE :: ByteString -> ByteString
englishReplacePCRE t =
  Replace.replaceAllCaptures Replace.TOP repl $ t PCREReplace.*=~ re
  where
    re = fromJust $ PCREReplace.compileRegex "Tom|Sawyer|Huckleberry|Finn"
    repl _ _ cap = case Replace.capturedText cap of
      "Tom"         -> Just "Huckleberry"
      "Sawyer"      -> Just "Finn"
      "Huckleberry" -> Just "Tom"
      "Finn"        -> Just "Sawyer"
      _             -> error "impossible"

--------------------
-- CaseFolding.txt
--------------------

caseFoldingTxt :: IO (Text, ByteString, String)
caseFoldingTxt = readBenchData "CaseFolding.txt"

data CaseFold
  = Common !Char !Char
  | Simple !Char !Char
  | Full !Char ![Char]
  | Turkic !Char !Char
  deriving (Eq, Show, Generic, NFData)

-- parser-regex
-- Note: parser-regex can be (and is, see scripts/GenCaseFold.hs) used for much
-- stricter parsing.
-- Since the same can't be said of other libraries, looser parsing is done to
-- match other libraries.
caseFoldingPR :: Text -> [CaseFold]
caseFoldingPR = RT.findAll one
  where
    hexCode = many (RT.char '0') *> fmap (chr . fromIntegral) RT.naturalHex
    manyHexCode = hexCode `RT.sepBy1` RT.char ' '
    one = F.asum
      [ Common <$> hexCode <* RT.text "; C; " <*> hexCode
      , Simple <$> hexCode <* RT.text "; S; " <*> hexCode
      , Full <$> hexCode <* RT.text "; F; " <*> manyHexCode
      , Turkic <$> hexCode <* RT.text "; T; " <*> hexCode
      ]

caseFoldingPRS :: String -> [CaseFold]
caseFoldingPRS = RL.findAll one
  where
    hexCode = many (RL.single '0') *> fmap (chr . fromIntegral) RL.naturalHex
    manyHexCode = hexCode `RL.sepBy1` RL.single ' '
    one = F.asum
      [ Common <$> hexCode <* RL.list "; C; " <*> hexCode
      , Simple <$> hexCode <* RL.list "; S; " <*> hexCode
      , Full <$> hexCode <* RL.list "; F; " <*> manyHexCode
      , Turkic <$> hexCode <* RL.list "; T; " <*> hexCode
      ]

-- regex-applicative
caseFoldingRA :: String -> [CaseFold]
caseFoldingRA = fromJust . RA.match re
  where
    re = toFindManyRA one
    hexCode = chr <$> RA.hexadecimal
    manyHexCode = liftA2 (:) hexCode (many (RA.sym ' ' *> hexCode))
    one = F.asum
      [ Common <$> hexCode <* RA.string "; C; " <*> hexCode
      , Simple <$> hexCode <* RA.string "; S; " <*> hexCode
      , Full <$> hexCode <* RA.string "; F; " <*> manyHexCode
      , Turkic <$> hexCode <* RA.string "; T; " <*> hexCode
      ]

-- regex-tdfa
caseFoldingTDFA :: Text -> [CaseFold]
caseFoldingTDFA = map (toCaseFold toc T.words) . RBase.matchAllText re
  where
    re :: TDFA.Regex
    re = RBase.makeRegexOpts RBase.blankCompOpt RBase.blankExecOpt $
         T.concat
         [ "(([0-9A-F]*); C; ([0-9A-F]*))|"
         , "(([0-9A-F]*); S; ([0-9A-F]*))|"
         , "(([0-9A-F]*); F; ([0-9A-F]*( [0-9A-F]*)*))|"
         , "(([0-9A-F]*); T; ([0-9A-F]*))"
         ]
    toc = chr . T.foldl' (\acc x -> acc * 16 + digitToInt x) 0

-- regex-pcre-builtin
caseFoldingPCRE :: ByteString -> [CaseFold]
caseFoldingPCRE = map (toCaseFold toc BC.words) . RBase.matchAllText re
  where
    re :: PCREBS.Regex
    re = RBase.makeRegexOpts PCREBS.compDotAll RBase.blankExecOpt $
         B.concat
         [ "(([0-9A-F]*); C; ([0-9A-F]*))|"
         , "(([0-9A-F]*); S; ([0-9A-F]*))|"
         , "(([0-9A-F]*); F; ([0-9A-F]*( [0-9A-F]*)*))|"
         , "(([0-9A-F]*); T; ([0-9A-F]*))"
         ]
    toc = chr . BC.foldl' (\acc x -> acc * 16 + digitToInt x) 0

-- Note: regex with only submatches is incapable of parsing the nested
-- space separated codes in the F case.
-- So the string is captured and parsed after the regex delivers its results.

toCaseFold
  :: (t -> Char) -- hex to Char
  -> (t -> [t]) -- words
  -> RBase.MatchText t
  -> CaseFold
toCaseFold toc ws m
   | Just _ <- idxMay 1 = Common (toc (idx 2)) (toc (idx 3))
   | Just _ <- idxMay 4 = Simple (toc (idx 5)) (toc (idx 6))
   | Just _ <- idxMay 7 = Full (toc (idx 8)) (tocs (idx 9))
   | otherwise          = Turkic (toc (idx 12)) (toc (idx 13))
  where
    idx i = fst (m ! i)
    idxMay i = let (t,(o,_)) = m ! i in if o == -1 then Nothing else Just t
    tocs = map toc . ws

--------
-- URI
--------

-- ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
-- A non-validating regex from
-- RFC 3986, "Uniform Resource Identifier (URI): Generic Syntax", Appendix B
-- https://datatracker.ietf.org/doc/html/rfc3986#appendix-B

htmlText :: IO (Text, ByteString, String)
htmlText = readBenchData "2014_in_video_games.html"

data URI t = URI
  { uScheme :: !(Maybe t)
  , uAuthority :: !(Maybe t)
  , uPath :: !t
  , uQuery :: !(Maybe t)
  , uFragment :: !(Maybe t)
  } deriving (Eq, Show, Generic, NFData)

uriT2BS :: URI Text -> URI ByteString
uriT2BS (URI s a p q f) = URI (eMay s) (eMay a) (TEnc.encodeUtf8 p) (eMay q) (eMay f)
  where
    eMay = fmap TEnc.encodeUtf8

uriS2T :: URI String -> URI Text
uriS2T (URI s a p q f) = URI (pMay s) (pMay a) (T.pack p) (pMay q) (pMay f)
  where
    pMay = fmap T.pack

-- parser-regex
uriPR :: Text -> [URI Text]
uriPR = RT.findAll inHref
  where
    inHref = RT.text "href=\"" *> uriR <* RT.char '"'
    uriR = URI
      <$> optional (RT.someTextOf (CS.not ":/?#\"") <* RT.char ':')
      <*> optional (RT.text "//" *> RT.manyTextOf (CS.not "/?#\""))
      <*> RT.manyTextOf (CS.not "^?#\"")
      <*> optional (RT.char '?' *> RT.manyTextOf (CS.not "#\""))
      <*> optional (RT.char '#' *> RT.manyTextOf (CS.not "\""))

uriPRS :: String -> [URI String]
uriPRS = RL.findAll inHref
  where
    inHref = RL.list "href=\"" *> uriR <* RL.single '"'
    uriR = URI
      <$> optional (RL.someStringOf (CS.not ":/?#\"") <* RL.single ':')
      <*> optional (RL.list "//" *> RL.manyStringOf (CS.not "/?#\""))
      <*> RL.manyStringOf (CS.not "^?#\"")
      <*> optional (RL.single '?' *> RL.manyStringOf (CS.not "#\""))
      <*> optional (RL.single '#' *> RL.manyStringOf (CS.not "\""))

-- regex-applicative
uriRA :: String -> [URI String]
uriRA = fromJust . RA.match re
  where
    re = toFindManyRA inHref
    inHref = RA.string "href=\"" *> uriR <* RA.sym '"'
    uriR = URI
      <$> optional (some (RA.psym (`CS.notMember` ":/?#\"")) <* RA.sym ':')
      <*> optional (RA.string "//" *> many (RA.psym (`CS.notMember` "/?#\"")))
      <*> many (RA.psym (`CS.notMember` "^?#\""))
      <*> optional (RA.sym '?' *> many (RA.psym (`CS.notMember` "#\"")))
      <*> optional (RA.sym '#' *> many (RA.psym (`CS.notMember` "\"")))

-- regex-tdfa
uriTDFA :: Text -> [URI Text]
uriTDFA = map toURI . RBase.matchAllText re
  where
    re :: TDFA.Regex
    re = RBase.makeRegexOpts RBase.blankCompOpt RBase.blankExecOpt
         ("href=\"(([^:/?#\"]+):)?(//([^/?#\"]*))?([^?#\"]*)(\\?([^#\"]*))?(#([^\"]*))?\"" :: Text)

-- regex-pcre-builtin
uriPCRE :: ByteString -> [URI ByteString]
uriPCRE = map toURI . RBase.matchAllText re
  where
    re :: PCREBS.Regex
    re = RBase.makeRegexOpts PCREBS.compDotAll RBase.blankExecOpt
         ("href=\"(([^:/?#\"]+):)?(//([^/?#\"]*))?([^?#\"]*)(\\?([^#\"]*))?(#([^\"]*))?\"" :: ByteString)

toURI :: RBase.MatchText t -> URI t
toURI m = URI (idxMay 2) (idxMay 4) (idx 5) (idxMay 7) (idxMay 9)
  where
    idx i = fst (m ! i)
    idxMay i = let (t,(o,_)) = m ! i in if o == -1 then Nothing else Just t

-----------------------------
-- Exponential backtracking
-----------------------------

expN :: Int
expN = 22

expText :: Text
expText = T.replicate expN "a"

expString :: String
expString = T.unpack expText

expBS :: ByteString
expBS = BC.replicate expN 'a'

-- parser-regex
expPR :: Text -> Bool
expPR = (== Just ()) . RT.parse p
  where
    p = RT.compile $
      replicateM_ expN (optional (RT.char 'a')) *>
      replicateM_ expN (RT.char 'a')

expPRS :: String -> Bool
expPRS = (== Just ()) . RL.parse p
  where
    p = RL.compile $
      replicateM_ expN (optional (RL.single 'a')) *>
      replicateM_ expN (RL.single 'a')

-- regex-applicative
expRA :: String -> Bool
expRA = (== Just ()) . f
  where
    f = RA.match $
      replicateM_ expN (optional (RA.sym 'a')) *>
      replicateM_ expN (RA.sym 'a')

-- regex-tdfa
expTDFA :: Text -> Bool
expTDFA = RBase.matchTest re
  where
    re :: TDFA.Regex
    re = RBase.makeRegexOpts RBase.blankCompOpt RBase.blankExecOpt
           ("^" <> T.replicate expN "a?" <> T.replicate expN "a" <> "$")

-- regex-pcre-builtin
expPCRE :: ByteString -> Bool
expPCRE = RBase.matchTest re
  where
    re :: PCREBS.Regex
    re = RBase.makeRegexOpts RBase.blankCompOpt RBase.blankExecOpt
           (BC.pack $ concat $ ["^"] <> replicate expN "a?" <> replicate expN "a" <> ["$"])

---------------
-- File utils
---------------

readBenchData :: String -> IO (Text, ByteString, String)
readBenchData name = do
  bs <- B.readFile $ "bench-data/" <> name <> ".data"
  let t = TEnc.decodeUtf8 bs
      b = TEnc.encodeUtf8 t
      s = T.unpack t
  pure (t,b,s)

----------------------------
-- regex-applicative utils
----------------------------

toFindManyRA :: RA.RE c a -> RA.RE c [a]
toFindManyRA re =
  reverse <$>
  RA.reFoldl RA.Greedy (flip ($)) [] ((:) <$> re <|> id <$ RA.anySym)

betweenCountRA :: (Int, Int) -> RA.RE c a -> RA.RE c [a]
betweenCountRA (l,h) re
  | l' > h = empty
  | otherwise = replicateAppendMRA l' re (go (h - l'))
  where
    l' = max l 0
    go 0 = pure []
    go n = liftA2 (:) re (go (n-1)) <|> pure []

-- n0 must be >= 0
replicateAppendMRA :: Int -> RA.RE c a -> RA.RE c [a] -> RA.RE c [a]
replicateAppendMRA n0 re re1 = go n0
  where
    go 0 = re1
    go n = liftA2 (:) re (go (n-1))

