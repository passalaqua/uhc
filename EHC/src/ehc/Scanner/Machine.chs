%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanning machine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Scanner machine, cloned from uulib, adapted for UHC.
The machinery is configured with |ScanOpts|, so it can be used for all parsers employed by UHC (e.g. also |Core|).
A |ScanOpts| specifies sets of keywords, variations on identifier parsing, etc etc.
%%]

%%[5 module {%{EH}Scanner.Machine} import(Data.Char,Data.List,Data.Maybe,System.IO,UU.Scanner.Position,UHC.Util.Utils,UHC.Util.ScanUtils,{%{EH}Scanner.Token})
%%]

%%[5 import(qualified Data.Set as Set)
%%]

%%[8 export(getRational)
%%]

%%[97 export(getBaseNumber)
%%]

-- debugging
%%[5 import(UHC.Util.Debug)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5
data ScanState
  = ScanState
      { ssAfterQual		:: Bool
      }

defaultScanState = ScanState False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5.scanHandle export(scanHandle)
scanHandle :: ScanOpts -> FilePath -> Handle -> IO [Token]
scanHandle opts fn fh
  = do  {  txt <- hGetContents fh
        ;  return (scan opts (initPos fn) txt)
        }
%%]

%%[5 export(scanFile,scan)
scanFile :: ScanOpts -> FilePath -> IO [Token]
scanFile opts fn =
        do txt <- readFile fn
           return (scan opts (initPos fn) txt)

scan :: ScanOpts -> Pos -> String -> [Token]
scan opts pos input
%%[[5
  = doScan defaultScanState pos input
%%][99
  = if scoLitmode opts
    then scanLitText defaultScanState pos input
    else doScan defaultScanState pos input
%%]]

 where
   -- locatein :: Ord a => [a] -> a -> Bool
   -- locatein es = isJust . btLocateIn compare (tab2tree (sort es))
   iskw     = (`Set.member` scoKeywordsTxt opts)
   iskwextra= scanpredIsKeywExtra opts
   isop     = (`Set.member` scoKeywordsOps opts)
   isSymbol = (`Set.member` scoSpecChars opts)
   isOpsym  = (`Set.member` scoOpChars opts)
   isPairSym= (`Set.member` scoSpecPairs opts)
   isStringDelim = (`elem` scoStringDelims opts)
%%[[99
   isPragma = (`Set.member` scoPragmasTxt opts) . map toUpper
%%]]

   isIdStart c = isLower    c || c == '_' || iskwextra c
   isIdChar  c = scanpredIsIdChar c || iskwextra c
   -- isQIdChar c = isIdChar   c || c == '.'

   allowQual   = scoAllowQualified opts

   scanIdent isId p s
     = (name,advc (length name) p,rest)
     where (name,rest) = span isId s
%%]

%%[18
   isUnboxed c = c == '#'
   scanUnboxed p s@(c:s') | isUnboxed c = (tokTpUnboxed, (++"#"), advc 1 p, s')
   scanUnboxed p s                      = (id          , id     ,        p, s )
%%]

%%[8
   scanDollarIdent :: String -> (String,Int,String)
   scanDollarIdent []           = ("",0,[])
   scanDollarIdent ('$':c:s)    | not (isSpace c)
                                = let (str,w,s') = scanDollarIdent s
                                  in  (c:str,w+2,s')
   scanDollarIdent cs@(c:s)     | isSpace c || isSymbol c || isOpsym c
                                = ("",0,cs)
   scanDollarIdent (c:s)        = let (str,w,s') = scanDollarIdent s
                                  in  (c:str,w+1,s')
%%]

%%[50
   scanQualified :: String -> ([String],String)
   scanQualified s
     = -- (\v -> tr "scanQualified" (s ++ ":"  ++ show v) v) $
       qual [] s
     where split isX s  = span (\c -> isX c && c /= '.') s
           validQuald c = isId c || isOpsym c
           isId       c = isIdStart c || isUpper c
           qual q s
             = case s of
                 (c:s') | isUpper c                         				-- possibly a module qualifier
                   -> case split isIdChar s' of
                        (s'',('.':srest@(c':_))) | validQuald c'  			-- something validly qualifiable follows
                          -> qual (q ++ [[c] ++ s'']) srest
                        _ -> dflt
                 (c:_) | isOpsym c || isIdChar c                  		-- not a qualifier, an operator or lowercase identifier
                   -> dflt
             where dflt = (q,s)
%%]

%%[5
   scanString :: Char -> Pos -> String -> (String,Pos,String)
   scanString d p []            = ("",p,[])
   scanString d p ('\\':'&':xs) = scanString d (advc 2 p) xs
   {-
   scanString d p ('\'':xs)     = let (str,p',r) = scanString d (advc 1 p) xs
                                  in  ('\'': str,p',r)
   -}
   scanString d p ( c  :xs) | isStringDelim c && c /= d
                                = let (str,p',r) = scanString d (advc 1 p) xs
                                  in  (c:str,p',r)
   scanString d p ('\\':x:xs) | isSpace x
                                = let (white,rest) = span isSpace xs
                                  in  case rest of
                                        ('\\':rest') -> scanString d (advc 1 $ foldl adv (advc 2 p) white) rest'
                                        _            -> ("",advc 2 p,xs)
   scanString d p xs = let (ch,cw,cr) = getchar d xs
                           (str,p',r) = scanString d (advc cw p) cr
                       in  maybe ("",p,xs) (\c -> (c:str,p',r)) ch
%%]

%%[99
   scanLitText scSt p ('\\':'b':'e':'g':'i':'n':'{':'c':'o':'d':'e':'}':s)
     | posIs1stColumn p
         = doScan scSt (advc 12 p) s
   scanLitText scSt p ('>':' ':s)
     | posIs1stColumn p
         = doScan scSt (advc 2 p) line ++ scanLitText scSt (advc (length line) p) rest
         where (line,rest) = break (== '\n') s
   scanLitText scSt p (c:s)
         = scanLitText scSt (adv p c) s
   scanLitText scSt p []
         = []
%%]

%%[5
   doScan scSt p [] = []
   doScan scSt p (c:s)     | isSpace c = let (sp,next) = span isSpace s
                                         in  doScan (scSt {ssAfterQual=False}) (foldl adv p (c:sp)) next

   doScan scSt p "--"  = []
   doScan scSt p ('-':'-':s@(c:_))
     | c == '-' || not (isOpsym c)
       = doScan (scSt {ssAfterQual=False}) p (dropWhile (/= '\n') s)
%%[[99
{-
-}
   doScan scSt p ('{':'-':'#':s)
     | isPragma pragma
       = reserved "{-#" p : reserved pragma p2 : doScan (scSt {ssAfterQual=False}) p3 s3
       where (w        ,s2) = getWhite s
             p2             = advc (length w) $ advc 3 p
             (pragma,p3,s3) = scanIdent isIdChar p2 s2
   doScan scSt p ('#':'-':'}':s) = reserved "#-}" p : doScan (scSt {ssAfterQual=False}) (advc 3 p) s
%%]]
   doScan scSt p ('{':'-':s)  = scanNestedComment doScan scSt (advc 2 p) s
   doScan scSt p (d:ss)
     | isStringDelim d
     = let (s,p',rest) = scanString d (advc 1 p) ss
       in if null rest || head rest /= d
             then errToken "Unterminated string literal" p : doScan scSt p' rest
             else valueToken TkString s p : doScan scSt (advc 1 p') (tail rest)
%%]

%%[8
   doScan scSt p ('$':ss)
     | scoDollarIdent opts   = tok : doScan scSt (advc (w+1) p) ss'
         where (ident,w,ss') = scanDollarIdent ss
               tok = if null ident
                     then errToken "Zero length $identifier" p
                     else valueToken TkVarid ident p
%%]

%%[99
   doScan scSt p ('\\':'e':'n':'d':'{':'c':'o':'d':'e':'}':s)
     | scoLitmode opts && posIs1stColumn p
         = scanLitText scSt (advc 10 p) s
%%]

%%[5
   -- this is experimental, for now, not foolproof, only to be used for the Prelude
   doScan scSt p ('\'':'\'':ss)
     = let (s,w,r) = scanDQuoteIdent ss
        in if null r
           then errToken "Unterminated double quote ident" p : doScan (scSt {ssAfterQual=False}) (advc (w+1) p) r
           else valueToken TkConid s p : doScan (scSt {ssAfterQual=False}) (advc (w+4) p) r

   doScan scSt p ('\'':ss)
     = let (mc,cwidth,rest) = scanChar ss
       in case mc of
            Nothing -> errToken "Error in character literal" p : doScan (scSt {ssAfterQual=False}) (advc cwidth p) rest
            Just c  -> if null rest || head rest /= '\''
                          then errToken "Unterminated character literal" p : doScan scSt (advc (cwidth+1) p) rest
                          else valueToken TkChar [c] p : doScan scSt (advc (cwidth+2) p) (tail rest)

   doScan scSt p cs@(c:c2:s)
     | isPairSym sym = reserved sym p : doScan (scSt {ssAfterQual=False}) (advc 2 p) s
         where sym = [c,c2]
   doScan scSt p cs@(c:s)
     | isSymbol c
                = reserved [c] p
                  : doScan (scSt {ssAfterQual=False}) (advc 1 p) s
%%]
%%[5
     | isIdStart c || isUpper c
         =
%%[[50
           let (qualPrefix,qualTail) = scanQualified cs
           in  if null qualPrefix || not allowQual
               then
%%]]
                    let (name', p', s') = scanIdent isIdChar (advc 1 p) s
                        name            = c:name'
                        nmiskw          = iskw name
                        (mktok,mknm,p'',s'')
%%[[18
                                             | not nmiskw = scanUnboxed p' s'
%%]]
                                             | otherwise  = (id,id,p',s')
                        tok             = if nmiskw
                                          then reserved name p
                                          else let n = mknm name
                                               in  valueToken (mktok $ varKind n) n p
                    in  tok : doScan scSt p'' s''
%%[[50
               else case doScan (scSt {ssAfterQual=True}) (advc (length qualPrefix + sum (map length qualPrefix)) p) qualTail of
                      (tok@(ValToken tp val _):toks)
                         -> ValToken (tokTpQual tp) (qualPrefix ++ val) p : toks
                      ts -> ts
%%]]
%%]
%%[5
     | isOpsym c
                 = let (name, s') = span isOpsym cs
                       tok n p (c:_)
                           | length suf' == 2 && isPairSym suf'
                                       = (fst (tok pre p []) ++ [reserved suf' (advc (length pre) p)],1)
                           where (pre,suf) = splitAt (length n - 1) n
                                 suf'      = suf ++ [c]
                       tok n p s
                           | isop n && not (ssAfterQual scSt)
                                       = ([reserved n p],0)
                           | length suf == 2 && isPairSym suf
                                       = (fst (tok pre p []) ++ [reserved suf (advc (length pre) p)],0)
                           | c==':'    = ([valueToken TkConOp n p],0)
                           | otherwise = ([valueToken TkOp n p],0)
                           where (pre,suf) = splitAt (length n - 2) n
                       (toks,drops) = tok name p s'
                   in toks ++ doScan (scSt {ssAfterQual=False}) (advc drops $ foldl adv p name) (drop drops s')
%%]
%%[8
     | scoAllowFloat opts && isDigit c
         = let (tktype,(number,mantissa,exp),w,cs') = getRational' cs
               m = maybe "" (\mant -> "." ++ mant)
               e = maybe "" (\(sign,exp) -> "E" ++ maybe "" id sign ++ exp)
           in  valueToken tktype (number ++ m mantissa ++ e exp) p
                 : doScan scSt (advc w p) cs'
%%]
%%[5
     | isDigit c = let (tktype,number,width,s') = getNumber cs
                   in  valueToken tktype number p : doScan scSt (advc width p) s'
%%]
%%[5
     | otherwise = errToken ("Unexpected character " ++ show c) p
                 : doScan scSt (adv p c) s
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5 export(scanpredIsIdChar, scanpredIsKeywExtra)
-- | Is base char for identifiers (and keywords)
scanpredIsIdChar :: Char -> Bool
scanpredIsIdChar  c = isAlphaNum c || c == '\'' || c == '_' -- -|| iskwextra c

-- | Is extra char for identifiers (and keywords)
scanpredIsKeywExtra :: ScanOpts -> Char -> Bool
scanpredIsKeywExtra opts = (`Set.member` scoKeywExtraChars opts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5
varKind :: String -> EnumValToken
varKind ('_':s)             = varKind s
varKind (c  :s) | isUpper c = TkConid
                | otherwise = TkVarid
varKind []                  = TkVarid

scanNestedComment
        :: (ScanState -> Pos -> String -> [Token])
        -> ScanState
        -> Pos
        -> String
        -> [Token]
scanNestedComment cont scSt pos inp = nest cont scSt pos inp
 where nest c scSt p ('-':'}':s) = c scSt (advc 2 p) s
       nest c scSt p ('{':'-':s) = nest (nest c) scSt (advc 2 p) s
       nest c scSt p (x:s)       = nest c scSt (adv p x) s
       nest _ _    _ []          = [ errToken "Unterminated nested comment" pos]


{-
scanString :: Char -> Pos -> String -> (String,Pos,String)
scanString d p []            = ("",p,[])
scanString d p ('\\':'&':xs) = scanString d (advc 2 p) xs
-- scanString d p ('\'':xs)     = let (str,p',r) = scanString d (advc 1 p) xs
--                                in  ('\'': str,p',r)
scanString d p ( c  :xs) | isStringDelim c && c /= d
                             = let (str,p',r) = scanString d (advc 1 p) xs
                               in  (c:str,p',r)
scanString d p ('\\':x:xs) | isSpace x
                             = let (white,rest) = span isSpace xs
                               in  case rest of
                                     ('\\':rest') -> scanString d (advc 1 $ foldl adv (advc 2 p) white) rest'
                                     _            -> ("",advc 2 p,xs)
scanString d p xs = let (ch,cw,cr) = getchar d xs
                        (str,p',r) = scanString d (advc cw p) cr
                    in  maybe ("",p,xs) (\c -> (c:str,p',r)) ch
-}

scanChar :: [Char] -> (Maybe Char,Int,[Char])
scanChar ('"' :xs) = (Just '"',1,xs)
scanChar xs        = getchar '\'' xs

getchar :: Char -> [Char] -> (Maybe Char,Int,[Char])
getchar d []          = (Nothing,0,[])
getchar d s@('\n':_ ) = (Nothing,0,s )
getchar d s@('\t':_ ) = (Nothing,0,s)
-- getchar d s@('\'':_ ) = (Nothing,0,s)
getchar d s@( c  :_ ) | c == d
                      = (Nothing,0,s)
getchar d   ('\\':xs) = let (c,l,r) = getEscChar xs
                        in (c,l+1,r)
getchar d (x:xs)      = (Just x,1,xs)
%%]

%%[99
getWhite :: String -> (String,String)
getWhite = span (`elem` " \t\r\n")
%%]

%%[5
scanDQuoteIdent :: String -> (String,Int,String)
scanDQuoteIdent []             = ("",0,[])
scanDQuoteIdent ('\'':'\'':xs) = ("",0,xs)
scanDQuoteIdent (x:xs)         = let (s,w,r) = scanDQuoteIdent xs -- should check similar to getchar
                                  in (x:s,w+1,r)

getEscChar :: [Char] -> (Maybe Char,Int,[Char])
getEscChar [] = (Nothing,0,[])
%%[[99
getEscChar s@('x':xs)           = let (tp,n,len,rest) = getNumber ('0' : s)
                                  in  (Just $ chr $ fromInteger $ getBaseNumber 16 n, len-1, rest)
getEscChar s@('o':xs)           = let (tp,n,len,rest) = getNumber ('0' : s)
                                  in  (Just $ chr $ fromInteger $ getBaseNumber 8  n, len-1, rest)
getEscChar s@('^':x:xs)         = case x `lookup` cntrCntrs of
                                    Just c -> (Just c,2,xs)
                                    _      -> (Nothing,0,s)
                                where cntrCntrs = [ ('@','\^@'), ('[','\^['), ('\\','\^\'), (']','\^]'), ('^','\^^'), ('_','\^_') ]
                                                  ++ zip ['A' .. 'Z'] ['\^A' .. '\^Z']
%%]]
getEscChar s@(x:xs) | isDigit x = let (tp,n,len,rest) = getNumber s
                                      val = case tp of
                                              TkInteger8  -> getBaseNumber 8  n
                                              TkInteger16 -> getBaseNumber 16 n
                                              TkInteger10 -> getBaseNumber 10 n
                                  in  if val >= 0 && val <= 255
                                         then (Just (chr $ fromInteger val),len, rest)
                                         else (Nothing,1,rest)
                    | otherwise = case x `lookup` cntrChars of
                                    Just c  -> (Just c,1,xs)
%%[[5
                                    Nothing -> (Nothing,0,s)
%%][99
                                    Nothing
                                      -> case filter (flip isPrefixOf s . fst) cntrStrs of
                                           [] -> (Nothing,0,s)
                                           ((m,mr):_)
                                              -> (Just mr,ml,drop ml s)
                                              where ml = length m
%%]]
  where cntrChars = [('a','\a'),('b','\b'),('f','\f'),('n','\n'),('r','\r'),('t','\t')
                    ,('v','\v'),('\\','\\'),('\"','\"'),('\'','\'')]
%%[[99
        cntrStrs  = [ ("NUL",'\NUL'), ("SOH",'\SOH'), ("STX",'\STX'), ("ETX",'\ETX')
                    , ("EOT",'\EOT'), ("ENQ",'\ENQ'), ("ACK",'\ACK'), ("BEL",'\BEL')
                    , ("BS" ,'\BS' ), ("HT" ,'\HT' ), ("LF" ,'\LF' ), ("VT" ,'\VT' )
                    , ("FF" ,'\FF' ), ("CR" ,'\CR' ), ("SO" ,'\SO' ), ("SI" ,'\SI' )
                    , ("DLE",'\DLE'), ("DC1",'\DC1'), ("DC2",'\DC2'), ("DC3",'\DC3')
                    , ("DC4",'\DC4'), ("NAK",'\NAK'), ("SYN",'\SYN'), ("ETB",'\ETB')
                    , ("CAN",'\CAN'), ("EM" ,'\EM' ), ("SUB",'\SUB'), ("ESC",'\ESC')
                    , ("FS" ,'\FS' ), ("GS" ,'\GS' ), ("RS" ,'\RS' ), ("US" ,'\US' )
                    , ("SP" ,'\SP' ), ("DEL",'\DEL')
                    ]
%%]]

getBaseNumber :: Integer -> [Char] -> Integer
getBaseNumber base n = foldl (\r x  -> toInteger (value x) + base * r) 0 n

getNumber :: [Char] -> (EnumValToken,[Char],Int,[Char])
getNumber cs@(c:s)
  | c /= '0'               = num10
  | null s                 = const0
  | hs == 'x' || hs == 'X' = num16
  | hs == 'o' || hs == 'O' = num8
  | otherwise              = num10
  where (hs:ts) = s
        const0 = (TkInteger10, "0",1,s)
        num10  = let (n,r) = span isDigit cs
                 in (TkInteger10,n,length n,r)
        num16   = readNum isHexaDigit  ts TkInteger16
        num8    = readNum isOctalDigit ts TkInteger8
        readNum p ts tk
          = let nrs@(n,rs) = span p ts
            in  if null n then const0
                          else (tk         , n, 2+length n,rs)
%%]

%%[8
getRational' :: String -> (EnumValToken,(String,Maybe String,Maybe (Maybe String,String)),Int,String)
getRational' s
  = case s2 of
      ('.':s3@(c:_)) | isDigit c && tktype == TkInteger10 && tktype2 == TkInteger10
        -> case scanExp s4 of
             Just (sign,number3,width3,s5)
               -> (TkFraction,(number,Just number2,Just (sign,number3)),width + width2 + width3 + 1,s5)
             _ -> (TkFraction,(number,Just number2,Nothing),width + width2 + 1,s4)
        where (tktype2,number2,width2,s4) = getNumber s3
      _ -> case scanExp s2 of
             Just (sign,number3,width3,s5)
               -> (TkFraction,(number,Nothing,Just (sign,number3)),width + width3,s5)
             _ -> (tktype,(number,Nothing,Nothing),width,s2)
  where (tktype,number,width,s2) = getNumber s
        scanExp s
          = case s of
              (c:s5) | c == 'e' || c == 'E'
                -> case s5 of
                     (csign:s6)
                       | csign == '+' || csign == '-'
                         -> case s6 of
                              (c:_) | isDigit c && tktype3 == TkInteger10
                                -> Just (Just [csign],number3,width3+2,s7)
                                where (tktype3,number3,width3,s7) = getNumber s6
                              _ -> Nothing
                       | isDigit csign && tktype3 == TkInteger10
                         -> Just (Nothing,number3,width3+1,s7)
                       where (tktype3,number3,width3,s7) = getNumber s5
                     _ -> Nothing
              _ -> Nothing

getRational :: String -> (String,Maybe String,Maybe (Maybe String,String))
getRational s
  = n
  where (_,n,_,_) = getRational' s
%%]

%%[5
isHexaDigit :: Char -> Bool
isHexaDigit  d = isDigit d || (d >= 'A' && d <= 'F') || (d >= 'a' && d <= 'f')

isOctalDigit :: Char -> Bool
isOctalDigit d = d >= '0' && d <= '7'

value :: Char -> Int
value c | isDigit c = ord c - ord '0'
        | isUpper c = ord c - ord 'A' + 10
        | isLower c = ord c - ord 'a' + 10
%%]
