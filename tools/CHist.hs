module CHist where

import Text

import Data.Int
import Data.List
import Data.Ord
import Debug.Trace
import Text.Printf
import qualified Data.Map.Strict      as DM


-- composition histogram
--   k = key          (e.g. CKey)
--   e = example type (e.g. String)
-- data CHist k e
data CHist k =
  CHist {
    chStaticInsts :: !Int64
  , chDynamicInsts :: !Int64
  , chValidKernels :: !Int64
  , chFilteredKernels :: !Int64
  , chErroneousKernels :: !Int64
  , chMap :: !(DM.Map k CBin)
  } deriving (Show,Read)

chEmpty :: CHist k
chEmpty =
  CHist {
    chStaticInsts = 0
  , chDynamicInsts = 0
  , chValidKernels = 0
  , chFilteredKernels = 0
  , chErroneousKernels = 0
  , chMap = DM.empty
  }

-- better show instance (for cached results)
fmtHist :: Show k => CHist k -> String
fmtHist ch =
  "CHist {\n" ++
  "  " ++ fmtF "chStaticInsts" chStaticInsts ++
  ", " ++ fmtF "chDynamicInsts" chDynamicInsts ++
  ", " ++ fmtF "chValidKernels" chValidKernels ++
  ", " ++ fmtF "chFilteredKernels" chFilteredKernels ++
  ", " ++ fmtF "chErroneousKernels" chErroneousKernels ++
  map_text ++
  "}\n"
  where fmtF fnm f = fnm ++ " = " ++ show (f ch) ++ "\n"

        map_text :: String
        map_text
          | DM.null (chMap ch) = "fromList []"
          | otherwise =
            ", chMap =\n" ++
            "    fromList [\n" ++
            "      " ++ fmtPair first ++
            concatMap (\x -> "    , " ++ fmtPair x) rest ++
            "    ]\n"
          where (first:rest) = DM.toList (chMap ch)
                fmtPair (k,cb) = "(" ++ show k ++ ",\n" ++
                    "        " ++ "CBin {\n" ++
                    "        " ++ "  cbStaticHits = " ++ show (cbStaticHits cb) ++ "\n" ++
                    "        " ++ ", cbDynamicHits = " ++ show (cbDynamicHits cb) ++ "\n" ++
                    examples ++
                    "        " ++ "})\n"
                  where examples
                          | null (cbExamples cb) = "        " ++ ", cbExamples = []\n"
                          | otherwise =
                            "        " ++ ", cbExamples = [\n" ++
                            "        " ++ "    " ++ show first ++ "\n" ++
                            concatMap (\x -> "        " ++ "  , " ++ show x ++ "\n") rest ++
                            "        " ++ "  ]\n"
                          where (first:rest) = cbExamples cb

-- composition key
--   op1 (K) tmp:t  src0:?  src1:?
--   op2 (K) dst:?  src2:?  tmp:t
-- ==>
--   (op2 . op1) (K) dst src0 src1 src2
--
-- data CKey =
--   CKey {
--     ckDstOp   :: !String -- e.g. "add" or "mov"
--   , ckDstType :: !String -- e.g. ":f" or ":f<-ud" (from mov:f<-ud
--   , ckSrcOp   :: !String
--   , ckSrcType :: !String
--   } deriving (Ord,Eq,Show,Read)
-- fmtCKey :: CKey -> String
-- fmtCKey ck =  "(" ++ ckDstOp ck ++ ckDstType ck ++ " (.) " ++ ckSrcOp ck ++ ckSrcType ck ++ ")"
-- composition bin
data CBin =
  CBin {
    cbStaticHits :: !Int64
  , cbDynamicHits :: !Int64
  , cbExamples :: ![CExample]
  } deriving (Show,Read)

data CExample =
  CExample {
    ceWorkload    :: !String
  , ceLocation    :: !(FilePath,Int)
  , cePc          :: !Int
  , ceHits        :: !Int64 -- for dynamic counts
  , ceSyntax      :: !String
  } deriving (Show,Read)

-------------------------------------------------------------------
--
-------------------------------------------------------------------
formatCHist :: Ord k => Int -> (k -> String) -> CHist k -> String
formatCHist reps fmtKey ch = fmtAlignedTable aligns cells
  where cells :: [[String]]
        cells = concatMap fmtBin bins ++ [totals]
          where totals =
                  [
                    padL 32 "TOTAL"
                  , ""
                  , printf "%12d           " total_static ++ maybe_dynamic
                  ]
                maybe_dynamic
                  | total_dynamic /= 0 = "  | " ++ printf "%12d" total_dynamic
                  | otherwise = ""

        aligns = [TextAlignLeft,TextAlignRight,TextAlignRight,TextAlignRight]

        total_static :: Int64
        total_static = chStaticInsts ch

        total_dynamic :: Int64
        total_dynamic = chDynamicInsts ch

        -- fmtBin :: (k,CBin) -> [[String]]
        fmtBin (ck,cb) =
            [
              [
                fmtKey ck
              , " | "
              , fmtPct total_static cbStaticHits ++ maybe_dynamic
              ]
            ] ++ fmtExamples reps cb
          where fmtPct :: Int64 -> (CBin -> Int64) -> String
                fmtPct total f = printf "%12d   (%5.2f%%)" (f cb) (100.0*pct)
                  where pct = toD (f cb) / toD total

                maybe_dynamic
                  | total_dynamic /= 0 = "  " ++ fmtPct total_dynamic cbDynamicHits
                  | otherwise = ""

        -- bins :: [(k,CBin)]
        bins = sortOn (Down . \(_,cb) -> cbDynamicHits cb) (DM.toList (chMap ch))

formatCHistWithClasses :: Ord k => [(String,k -> Bool)] -> Int -> (k -> String) -> CHist k -> String
formatCHistWithClasses [] reps fmtKey ch = formatCHist reps fmtKey ch
formatCHistWithClasses classes reps fmtKey ch = fmtAlignedTable aligns table_cells
  where table_cells :: [[String]]
        table_cells = concatMap fmtClass equiv_classes ++ [totals]
          where totals =
                  [
                    ""
                  , padL 32 "TOTAL"
                  , " | "
                  , printf "%12d           " total_static ++ maybe_dynamic
                  ,  "  |"
                  ]

                maybe_dynamic
                  | total_dynamic /= 0 = "  | " ++ printf "%12d" total_dynamic
                  | otherwise = ""

        some_key_length :: Int
        some_key_length
          | DM.null (chMap ch) = 10
          | otherwise = length (fmtKey (fst (head (DM.toList (chMap ch)))))

        aligns = [TextAlignLeft,TextAlignLeft,TextAlignRight,TextAlignRight,TextAlignRight]

        -- fmtClass :: Ord k => (String,[(k,CBin)]) -> [[String]]
        fmtClass (class_name,bs) = class_total:concatMap fmtBin (sortBins bs)
          where class_total :: [String]
                class_total =
                  [
                    class_name
                  , replicate some_key_length '-'
                  , "-+-"
                  , fmtPct total_static class_static ++ maybe_dynamic
                  , " -+"
                  ]
                  where maybe_dynamic
                          | total_dynamic /= 0 = " -+-" ++ fmtPct total_dynamic class_dynamic
                          | otherwise = ""

                class_static :: Int64
                class_static = foldl' (\n (_,cb) -> n + cbStaticHits cb) 0 bs
                class_dynamic :: Int64
                class_dynamic = foldl' (\n (_,cb) -> n + cbDynamicHits cb) 0 bs

                fmtBin (k,cb) =
                    [
                      [
                        ""
                      , fmtKey k
                      , " | "
                      , fmtPctF total_static cbStaticHits ++ maybe_dynamic
                      , "  |"
                      ]
                    ] ++ fmtExamples reps cb
                  where fmtPctF :: Int64 -> (CBin -> Int64) -> String
                        fmtPctF total f = fmtPct total (f cb)

                        maybe_dynamic
                          | total_dynamic /= 0 = "  | " ++ fmtPctF total_dynamic cbDynamicHits
                          | otherwise = ""

                fmtPct :: Int64 -> Int64 -> String
                fmtPct total k = printf "%12d   (%5.2f%%)" k (100.0*pct)
                  where pct = toD k / toD total

        total_static, total_dynamic :: Int64
        total_static = chStaticInsts ch
        total_dynamic = chDynamicInsts ch

        -- equiv_classes :: [(String,[(k,CBin)])]
        equiv_classes = partitionToEquivalenceClasses classes (DM.toList (chMap ch))

toD :: Integral i => i -> Double
toD = fromIntegral

sortBins :: Ord k => [(k,CBin)] -> [(k,CBin)]
sortBins bs
  | all ((==0) . cbDynamicHits . snd) bs = sortOn (Down . \(_,cb) -> cbStaticHits cb) bs
  | otherwise = sortOn (Down . \(_,cb) -> cbDynamicHits cb) bs

{-
chPartition :: Ord k => [k -> Bool)] -> CHist k -> [CHist k]
chPartition classes ch = classes_unsorted
  where classes_unsorted = loop classes (DM.toList ch)
          where loop (f:fs) bs = (class_name,bs_in) : loop classes bs_out
                  where (bs_in,bs_out) = partition  bs
                loop [] [] = []
                loop [] bs = [("<Other>",bs)]
-}
chBins :: CHist k -> [(k,CBin)]
chBins = DM.toList . chMap

partitionToEquivalenceClasses ::
  Ord k => [(String,k -> Bool)] -> [(k,CBin)] -> [(String,[(k,CBin)])]
partitionToEquivalenceClasses classes bs = classes_unsorted
  where classes_unsorted = loop classes bs
          where loop ((class_name,fn):classes) bs = (class_name,bs_in) : loop classes bs_out
                  where (bs_in,bs_out) = partition (fn . fst) bs
                loop [] [] = []
                loop [] bs = [("<Other>",bs)]


fmtExamples :: Int -> CBin -> [[String]]
fmtExamples reps cb =
    concatMap fmtExample $ sortOn cmp (dropSome (cbExamples cb))
  where cmp ce = (ceWorkload ce, cePc ce)
        dropSome :: [CExample] -> [CExample]
        dropSome
          | reps < 0 = id
          | otherwise = take reps

fmtExample :: CExample -> [[String]]
fmtExample ce =
    [
--      [ceWorkload ce, " | ",
--        show (cePc ce) ++        ":      "     ++ "  " ++
--          printf "%-8s   " ("#" ++ show (ceHits ce)) ++
--            ceSyntax ce]
      ["","","",ceSyntax ce, ceWorkload ce]
    ]


chInsert :: Ord k => Int -> k -> CBin -> CHist k -> CHist k
chInsert reps ck cb ch = ch{chMap = DM.insertWith combine ck cb (chMap ch)}
  where combine :: CBin -> CBin -> CBin
        combine cb_new cb_old =
            cb_old {
              cbStaticHits  = cbStaticHits cb_old + cbStaticHits cb_new
            , cbDynamicHits = cbDynamicHits cb_old + cbDynamicHits cb_new
            , cbExamples    = merged_examples
            }
          where merged_examples
                  | reps < 0  = cbExamples cb_new ++ cbExamples cb_old
                  | otherwise = take reps $ cbExamples cb_new ++ cbExamples cb_old

chMerge :: Ord k => Int -> CHist k -> CHist k -> CHist k
chMerge reps ch1 ch2 =
    ch1 {
      chMap          = DM.unionWith cbMerge (chMap ch1) (chMap ch2)
    , chStaticInsts  = chStaticInsts ch1 + chStaticInsts ch2
    , chDynamicInsts = chDynamicInsts ch1 + chDynamicInsts ch2

    , chValidKernels = chValidKernels ch1 + chValidKernels ch2
    , chFilteredKernels = chFilteredKernels ch1 + chFilteredKernels ch2
    , chErroneousKernels = chErroneousKernels ch1 + chErroneousKernels ch2
    }
  where cbMerge :: CBin -> CBin -> CBin
        cbMerge cb1 cb2 =
            CBin {
              cbStaticHits = cbStaticHits cb1 + cbStaticHits cb2
            , cbDynamicHits = cbDynamicHits cb1 + cbDynamicHits cb2
            , cbExamples = merged_examples
            }
          where merged_examples
                  | reps < 0  = cbExamples cb1 ++ cbExamples cb2
                  | otherwise = take reps (cbExamples cb1 ++ cbExamples cb2)


formatCHistWithClassesHtml :: Ord k => [(String,k -> Bool)] -> Int -> (k -> String) -> CHist k -> String
formatCHistWithClassesHtml classes reps fmtKey ch = html
  where total_static, total_dynamic :: Int64
        total_static = chStaticInsts ch
        total_dynamic = chDynamicInsts ch

        -- equiv_classes :: [(String,[(k,CBin)])]
        equiv_classes = partitionToEquivalenceClasses classes (DM.toList (chMap ch))

        html :: String
        html =
          "<body onload='loadHandler()'>\n" ++
          "<style>\n" ++
          -- "table, th, td {border: 1px gray solid; font-family: monospace}\n" ++
          ".equiv {border: 3px black solid; font-family: monospace}\n" ++
          ".category {border: 1px black dashed; font-family: monospace}\n" ++
          ".nums {border: 1px black dashed; font-family: monospace}\n" ++
          ".example_table {background-color:rgb(240,240,256); color:rgb(96,96,96); width:100%}\n" ++
          ".example_syntax {font-family: monospace}\n" ++
          ".example_source {font-family: monospace; font-color:rgb(192,192,192)}\n" ++
          "</style>\n" ++
          "<script>\n" ++
          "const examples = new Map();\n" ++
          "function loadHandler()\n" ++
          "{\n" ++
            "const t = document.getElementById('hist')\n" ++
            "for (var r_ix = t.rows.length - 1; r_ix > 0; r_ix--) {\n" ++
          "    const r = t.rows[r_ix]\n" ++
          "    if (r.id && r.id.startsWith('e.')) {\n" ++
          "      examples[r.id.substring(2)] = r\n" ++
          "      t.deleteRow(r_ix)\n" ++
          "    }\n" ++
          "  }\n" ++
          "}\n" ++
          "function toggleExamples(tr_id)\n" ++
          "{\n" ++
          "  // 's.' + tr_id is the stats row\n" ++
          "  // 'e.' + tr_id is the examples row\n" ++
          "  // console.log(\"toggle \" + tr_id)\n" ++
          "\n" ++
          "  const t = document.getElementById('hist')\n" ++
          "\n" ++
          "  // remove it if it's in the table\n" ++
          "  // otherwise if we hit the stats row (above it), then we re-insert the row\n" ++
          "  for (var r_ix = t.rows.length - 1; r_ix > 0; r_ix--) {\n" ++
          "    const r = t.rows[r_ix]\n" ++
          "    if (r.id == 'e.' + tr_id) {\n" ++
          "      t.deleteRow(r_ix)\n" ++
          "      return\n" ++
          "    } else if (r.id == 's.' + tr_id) {\n" ++
          "      const e = examples[tr_id]\n" ++
          "      const e1 = t.insertRow(r_ix + 1)\n" ++
          "      e1.innerHTML = e.innerHTML\n" ++
          "      e1.id = e.id\n" ++
          "    }\n" ++
          "  }\n" ++
          "}\n" ++
          "</script>\n" ++
          "<table id=\"hist\" style=\"width:100%\">\n" ++
          "  <tr><th colspan=\"2\" class=\"equiv\">Key</th><th colspan=\"2\" class=\"equiv\">Frequency</th></tr>\n" ++
          concatMap fmtClass (zip [0..] equiv_classes) ++
          "</table>\n" ++
          "</body>\n" ++
          ""

        fmtClass (cls_ix,(cls_nm,bins)) =
            "  <tr>" ++
                "<td class=\"equiv\" colspan=\"2\">" ++ escHtml cls_nm ++ "</td>" ++
                fmtPctTds total_static class_static ++ "</tr>\n" ++
            concatMap fmtBin (zip [0..] bins) ++ "\n"
          where class_static :: Int64
                class_static = foldl' (\n (_,cb) -> n + cbStaticHits cb) 0 bins

                fmtBin (b_ix,(k,cb)) =
                    "  <tr id=\"s." ++ e_id ++ "\" onclick='toggleExamples(\"" ++ e_id ++ "\")'>\n" ++
                    "    <td> </td>" ++
                    "<td class=\"category\">" ++ escHtml (fmtKey k) ++ "</td>" ++
                    fmtPctTds total_static (cbStaticHits cb) ++ "</tr>\n" ++
                    "  <tr id=\"e." ++ e_id ++ "\">\n" ++
                    "    <td colspan=\"4\" onclick='toggleExamples(\"" ++ e_id ++ "\")' style=\"overflow:scroll\">\n" ++
                    example_table ++
                    "    </td></tr>\n" ++
                    ""
                  where e_id = "c" ++ show cls_ix ++ ".b" ++ show b_ix

                        example_table = -- ""
                          "    <table class=\"example_table\">\n" ++ concatMap fmtExampleTr (orderAndPrune cb) ++ "    </table>\n"

                        fmtExampleTr ce =
                          "    <tr><td class=\"example_syntax\">" ++ escHtml (ceSyntax ce) ++ "</td>" ++
                          "<td class=\"example_source\">" ++ escHtml (ceWorkload ce) ++ "</td></tr>\n"

                        -- orderAndPrune :: CBin k -> [CExample]
                        orderAndPrune = sortOn cmp . dropSome . cbExamples
                          where cmp ce = (ceWorkload ce, cePc ce)
                                dropSome :: [CExample] -> [CExample]
                                dropSome
                                  | reps < 0 = id
                                  | otherwise = take reps

        fmtPctTds :: Int64 -> Int64 -> String
        fmtPctTds total k =
            "<td class=\"nums\">" ++ frq_str ++ "</td>" ++
              "<td class=\"nums\" style=\"width:128px\">" ++ pct_str ++ "</td>" ++
                ""
          where pct = toD k / toD total
                frq_str = printf "%12d" k
                pct_str = printf "%5.2f%%"  (100.0*pct)


escHtml :: String -> String
escHtml = loop
  where loop [] = []
        loop (c:cs) = e ++ loop cs
          where e =
                  case c of
                    '<' -> "&lt;"
                    '>' -> "&gt;"
                    '&' -> "&amp;"
                    '"' -> "&quot;"
                    ' ' -> "&nbsp;"
                    _ -> [c]
