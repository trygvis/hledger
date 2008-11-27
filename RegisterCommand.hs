{-| 

A ledger-compatible @register@ command.

-}

module RegisterCommand
where
import Ledger
import Options


-- | Print a register report.
register :: [Opt] -> [String] -> Ledger -> IO ()
register opts args l = putStr $ showRegisterReport opts args l

{- |
Generate the register report. Each ledger entry is displayed as two or
more lines like this:

@
date (10)  description (20)     account (22)            amount (11)  balance (12)
DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
                                aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
                                ...                     ...         ...
@
-}
showRegisterReport :: [Opt] -> [String] -> Ledger -> String
showRegisterReport opts args l = showtxns ts nulltxn nullmixedamt
    where
      ts = filter matchapats $ ledgerTransactions l
      matchapats t = matchpats apats $ account t
      apats = fst $ parseAccountDescriptionArgs opts args
      matchdisplayopt Nothing t = True
      matchdisplayopt (Just e) t = (fromparse $ parsewith datedisplayexpr e) t
      dopt = displayFromOpts opts

      -- show display-filtered transactions, one per line, with a running balance
      showtxns [] _ _ = ""
      showtxns (t@Transaction{amount=a}:ts) tprev bal =
          (if (isZeroMixedAmount a || (not $ matchdisplayopt dopt t)) then "" else this) ++ showtxns ts t bal'
          where
            this = showtxn (t `issame` tprev) t bal'
            issame t1 t2 = entryno t1 == entryno t2
            bal' = bal + amount t

      -- show one transaction line, with or without the entry details
      showtxn :: Bool -> Transaction -> MixedAmount -> String
      showtxn omitdesc t b = concatBottomPadded [entrydesc ++ txn ++ " ", bal] ++ "\n"
          where
            entrydesc = if omitdesc then replicate 32 ' ' else printf "%s %s " date desc
            date = showDate $ da
            desc = printf "%-20s" $ elideRight 20 de :: String
            txn = showRawTransaction $ RawTransaction a amt "" tt
            bal = padleft 12 (showMixedAmountOrZero b)
            Transaction{date=da,description=de,account=a,amount=amt,ttype=tt} = t

