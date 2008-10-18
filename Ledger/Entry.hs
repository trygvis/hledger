{-|

An 'Entry' represents a regular entry in the ledger file. It normally
contains two or more balanced 'RawTransaction's.

-}

module Ledger.Entry
where
import Ledger.Utils
import Ledger.Types
import Ledger.RawTransaction
import Ledger.Amount


instance Show Entry where show = showEntry

{-|
Show a ledger entry, formatted for the print command. ledger 2.x's
standard format looks like this:

@
yyyy/mm/dd[ *][ CODE] description.........          [  ; comment...............]
    account name 1.....................  ...$amount1[  ; comment...............]
    account name 2.....................  ..$-amount1[  ; comment...............]

pcodewidth    = no limit -- 10
pdescwidth    = no limit -- 20
pacctwidth    = 35 minimum, no maximum
pamtwidth     = 11
pcommentwidth = no limit -- 22
@
-}
showEntry :: Entry -> String
showEntry e = 
    unlines $ [precedingcomment ++ description] ++ (showtxns $ etransactions e) ++ [""]
    where
      precedingcomment = epreceding_comment_lines e
      description = concat [date, status, code, desc] -- , comment]
      date = showDate $ edate e
      status = if estatus e then " *" else ""
      code = if (length $ ecode e) > 0 then (printf " (%s)" $ ecode e) else ""
      desc = " " ++ edescription e
      comment = if (length $ ecomment e) > 0 then "  ; "++(ecomment e) else ""
      showtxns (t1:t2:[]) = [showtxn t1, showtxnnoamt t2]
      showtxns ts = map showtxn ts
      showtxn t = showacct t ++ "  " ++ (showamount $ tamount t) ++ (showcomment $ tcomment t)
      showtxnnoamt t = showacct t ++ "              " ++ (showcomment $ tcomment t)
      showacct t = "    " ++ (showaccountname $ taccount t)
      showamount = printf "%12s" . showAmount
      showaccountname s = printf "%-34s" s
      showcomment s = if (length s) > 0 then "  ; "++s else ""

showDate = printf "%-10s"

-- | Raise an error if this entry is not balanced.
assertBalancedEntry :: Entry -> Entry
assertBalancedEntry e
    | isEntryBalanced e = e
    | otherwise = error $ "transactions don't balance in:\n" ++ show e

isEntryBalanced :: Entry -> Bool
isEntryBalanced (Entry {etransactions=ts}) = isZeroMixedAmount sum
    where
      sum = sumRawTransactions realts
      realts = filter isReal ts

-- | Fill in a missing balance in this entry, if there is one, 
-- or raise an error if there is more than one.
autofillEntry :: Entry -> Entry
autofillEntry e@(Entry {etransactions=ts}) = e{etransactions=ts'}
    where ts' = fromMaybe 
                (error $ "too many missing amounts in this entry, could not auto-balance:\n" ++ show e)
                (autofillTransactions ts)

-- modifier & periodic entries

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

