import Data.Char
import Text.Printf


-- _Z5add64PxPKxx
-- add64(long long*, long long const*, long long)

type P a = Either (Int,String) a

demangle :: String :: Either (Int,String) String
demangle ('_':'Z':sfx0) =
    case span isDigit sfx of
      ("",_) -> Left (2,"expected digits")
      (ds,sfx) ->
        case splitAt (read ds :: Int) sfx of
          ("",_) -> Left (2+length ds,"empty function name")
          (fnm,sfx) ->
            (\as -> fnm ++ "(" ++ intercalate ", " as ++ ")") <$>
              readArgs (2+length ds+length fnm) sfx
  where fatalAtSfx :: String -> String -> P a
        fatalAtSfx sfx msg = Left (length sfx0 - length sfx, msg)

        readArg :: String -> P (String,String)
        readArg "" = fatalAtSfx "" "expected arg"
        readArg ('K':x) = do
          (a,sfx) <- readArg
          return ("const " ++ a, sfx)
        readArg ('V':x) = do
          (a,sfx) <- readArg
          return ("volatile " ++ a, sfx)
        readArg ('P':x) =
          (a,sfx) <- readArg
          return (a ++ "*", sfx)
        readArg ('R':x) =
          (a,sfx) <- readArg
          return (a ++ "&", sfx)
        readArg (c:sfx) =
          case c `lookup` modTable of
            -- c++filt does it in this order for some odd reason
            -- e.g. you can have: int const, rather than const int
            Just x -> (++(" "++x)) <$> readArg sfx
            Nothing ->
              case c `lookup` suffixTable of
                Just x -> (++x) <$> readArg sfx
                Nothing ->
                  case c `lookup` primTable of
                    Nothing -> fatalAtSfx (c:sfx) (c:": unknown primitive type")
                    Just x -> (x,sfx)
              return ("long long",sfx)

        modTable :: [(Char,String)]
        modTable =
          [
            prim 'K' "const"
          , prim 'V' "volatile"
          ]

        suffixTable :: [(Char,String)]
        suffixTable =
          [
            prim 'P' "*"
          , prim 'R' "&"
          ]

        primTable :: [(Char,String)]
        primTable =
            [
              prim 'a' "signed char"
            , prim 'b' "bool"
            , prim 'c' "char"
            , prim 'd' "double"
            , prim 'e' "long double"
            , prim 'f' "float"
            , prim 'g' "__float128"
            , prim 'h' "unsigned char"
            , prim 'i' "int"
            , prim 'j' "unsigned int"
            , prim 'l' "long"
            , prim 'm' "unsigned long"
            , prim 'n' "_int128"
            , prim 'o' "unsigned _int128"
            , prim 's' "short"
            , prim 't' "unsigned short"
            , prim 'v' "void"
            , prim 'w' "wchar_t"
            , prim 'x' "long long"
            , prim 'y' "unsigned long long"
            , prim 'z' "..."
            ]
          where prim = (,)

        readArgs :: String -> P [String]
        readArgs "" = return []
        readArgs as = do
          (a,sfx) <- readArg as
          (a:) <$> readArgs sfx



