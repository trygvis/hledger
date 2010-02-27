{-|

A 'Transaction' represents a single balanced entry in the ledger file. It
normally contains two or more balanced 'Posting's.

-}

module Ledger.Transaction
where
import Ledger.Utils
import Ledger.Types
import Ledger.Dates
import Ledger.Posting
import Ledger.Amount

instance Show Transaction where show = showTransactionUnelided

instance Show ModifierTransaction where 
    show t = "= " ++ mtvalueexpr t ++ "\n" ++ unlines (map show (mtpostings t))

instance Show PeriodicTransaction where 
    show t = "~ " ++ ptperiodicexpr t ++ "\n" ++ unlines (map show (ptpostings t))

nulltransaction :: Transaction
nulltransaction = Transaction {
                    tdate=nulldate,
                    teffectivedate=Nothing, 
                    tstatus=False, 
                    tcode="", 
                    tdescription="", 
                    tcomment="",
                    tpostings=[],
                    tpreceding_comment_lines=""
                  }

{-|
Show a ledger entry, formatted for the print command. ledger 2.x's
standard format looks like this:

@
yyyy/mm/dd[ *][ CODE] description.........          [  ; comment...............]
    account name 1.....................  ...$amount1[  ; comment...............]
    account name 2.....................  ..$-amount1[  ; comment...............]

pcodewidth    = no limit -- 10          -- mimicking ledger layout.
pdescwidth    = no limit -- 20          -- I don't remember what these mean,
pacctwidth    = 35 minimum, no maximum  -- they were important at the time.
pamtwidth     = 11
pcommentwidth = no limit -- 22
@
-}
showTransaction :: Transaction -> String
showTransaction = showTransaction' True False

showTransactionUnelided :: Transaction -> String
showTransactionUnelided = showTransaction' False False

showTransactionForPrint :: Bool -> Transaction -> String
showTransactionForPrint effective = showTransaction' False effective

showTransaction' :: Bool -> Bool -> Transaction -> String
showTransaction' elide effective t =
    unlines $ [description] ++ showpostings (tpostings t) ++ [""]
    where
      description = concat [date, status, code, desc, comment]
      date | effective = showdate $ fromMaybe (tdate t) $ teffectivedate t
           | otherwise = showdate (tdate t) ++ maybe "" showedate (teffectivedate t)
      status = if tstatus t then " *" else ""
      code = if length (tcode t) > 0 then printf " (%s)" $ tcode t else ""
      desc = ' ' : tdescription t
      comment = if null com then "" else "  ; " ++ com where com = tcomment t
      showdate = printf "%-10s" . showDate
      showedate = printf "=%s" . showdate
      showpostings ps
          | elide && length ps > 1 && isTransactionBalanced t
              = map showposting (init ps) ++ [showpostingnoamt (last ps)]
          | otherwise = map showposting ps
          where
            showposting p = showacct p ++ "  " ++ showamount (pamount p) ++ showcomment (pcomment p)
            showpostingnoamt p = rstrip $ showacct p ++ "              " ++ showcomment (pcomment p)
            showacct p = "    " ++ showstatus p ++ printf (printf "%%-%ds" w) (showAccountName Nothing (ptype p) (paccount p))
            w = maximum $ map (length . paccount) ps
            showamount = printf "%12s" . showMixedAmount
            showcomment s = if null s then "" else "  ; "++s
            showstatus p = if pstatus p then "* " else ""

-- | Show an account name, clipped to the given width if any, and
-- appropriately bracketed/parenthesised for the given posting type.
showAccountName :: Maybe Int -> PostingType -> AccountName -> String
showAccountName w = fmt
    where
      fmt RegularPosting = take w'
      fmt VirtualPosting = parenthesise . reverse . take (w'-2) . reverse
      fmt BalancedVirtualPosting = bracket . reverse . take (w'-2) . reverse
      w' = fromMaybe 999999 w
      parenthesise s = "("++s++")"
      bracket s = "["++s++"]"

realPostings :: Transaction -> [Posting]
realPostings = filter isReal . tpostings

virtualPostings :: Transaction -> [Posting]
virtualPostings = filter isVirtual . tpostings

balancedVirtualPostings :: Transaction -> [Posting]
balancedVirtualPostings = filter isBalancedVirtual . tpostings

-- | Get the sums of a transaction's real, virtual, and balanced virtual postings.
transactionPostingBalances :: Transaction -> (MixedAmount,MixedAmount,MixedAmount)
transactionPostingBalances t = (sumPostings $ realPostings t
                               ,sumPostings $ virtualPostings t
                               ,sumPostings $ balancedVirtualPostings t)

-- | Is this transaction balanced ? A balanced transaction's real
-- (non-virtual) postings sum to 0, and any balanced virtual postings
-- also sum to 0.
isTransactionBalanced :: Transaction -> Bool
isTransactionBalanced t = isReallyZeroMixedAmountCost rsum && isReallyZeroMixedAmountCost bvsum
    where (rsum, _, bvsum) = transactionPostingBalances t

-- | Ensure that this entry is balanced, possibly auto-filling a missing
-- amount first. We can auto-fill if there is just one non-virtual
-- transaction without an amount. The auto-filled balance will be
-- converted to cost basis if possible. If the entry can not be balanced,
-- return an error message instead.
balanceTransaction :: Transaction -> Either String Transaction
balanceTransaction t@Transaction{tpostings=ps}
    | length missingamounts' > 1 = Left $ printerr "could not balance this transaction, too many missing amounts"
    | not $ isTransactionBalanced t' = Left $ printerr $ nonzerobalanceerror t'
    | otherwise = Right t'
    where
      (withamounts, missingamounts) = partition hasAmount $ filter isReal ps
      (_, missingamounts') = partition hasAmount ps
      t' = t{tpostings=ps'}
      ps' | length missingamounts == 1 = map balance ps
          | otherwise = ps
          where 
            balance p | isReal p && not (hasAmount p) = p{pamount = costOfMixedAmount (-otherstotal)}
                      | otherwise = p
                      where otherstotal = sum $ map pamount withamounts
      printerr s = printf "%s:\n%s" s (showTransactionUnelided t)

nonzerobalanceerror :: Transaction -> String
nonzerobalanceerror t = printf "could not balance this transaction (%s%s%s)" rmsg sep bvmsg
    where
      (rsum, _, bvsum) = transactionPostingBalances t
      rmsg | isReallyZeroMixedAmountCost rsum = ""
           | otherwise = "real postings are off by " ++ show rsum
      bvmsg | isReallyZeroMixedAmountCost bvsum = ""
            | otherwise = "balanced virtual postings are off by " ++ show bvsum
      sep = if not (null rmsg) && not (null bvmsg) then "; " else ""

-- | Convert the primary date to either the actual or effective date.
ledgerTransactionWithDate :: WhichDate -> Transaction -> Transaction
ledgerTransactionWithDate ActualDate t = t
ledgerTransactionWithDate EffectiveDate t = txnTieKnot t{tdate=fromMaybe (tdate t) (teffectivedate t)}
    

-- | Ensure a transaction's postings refer back to it.
txnTieKnot :: Transaction -> Transaction
txnTieKnot t@Transaction{tpostings=ps} = t{tpostings=map (settxn t) ps}

-- | Set a posting's parent transaction.
settxn :: Transaction -> Posting -> Posting
settxn t p = p{ptransaction=Just t}

