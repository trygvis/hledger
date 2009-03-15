{-| 

A ledger-compatible @balance@ command. 

ledger's balance command is easy to use but not easy to describe
precisely.  In the examples below we'll use sample.ledger, which has the
following account tree:

@
 assets
   bank
     checking
     saving
   cash
 expenses
   food
   supplies
 income
   gifts
   salary
 liabilities
   debts
@

The balance command shows accounts with their aggregate balances.
Subaccounts are displayed indented below their parent. Each balance is the
sum of any transactions in that account plus any balances from
subaccounts:

@
 $ hledger -f sample.ledger balance
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
                  $2  expenses
                  $1    food
                  $1    supplies
                 $-2  income
                 $-1    gifts
                 $-1    salary
                  $1  liabilities:debts
@

Usually, the non-interesting accounts are elided or omitted. Above,
@checking@ is omitted because it has no subaccounts and a zero balance.
@bank@ is elided because it has only a single displayed subaccount
(@saving@) and it would be showing the same balance as that ($1). Ditto
for @liabilities@. We will return to this in a moment.

The --depth argument can be used to limit the depth of the balance report.
So, to see just the top level accounts:

@
$ hledger -f sample.ledger balance --depth 1
                 $-1  assets
                  $2  expenses
                 $-2  income
                  $1  liabilities
@

This time liabilities has no displayed subaccounts (due to --depth) and
is not elided.

With one or more account pattern arguments, the balance command shows
accounts whose name matches one of the patterns, plus their parents
(elided) and subaccounts. So with the pattern o we get:

@
 $ hledger -f sample.ledger balance o
                  $1  expenses:food
                 $-2  income
                 $-1    gifts
                 $-1    salary
--------------------
                 $-1
@

The o pattern matched @food@ and @income@, so they are shown. Unmatched
parents of matched accounts are also shown (elided) for context (@expenses@).

Also, the balance report shows the total of all displayed accounts, when
that is non-zero. Here, it is displayed because the accounts shown add up
to $-1.

Here is a more precise definition of \"interesting\" accounts in ledger's
balance report:

- an account which has just one interesting subaccount branch, and which
  is not at the report's maximum depth, is interesting if the balance is
  different from the subaccount's, and otherwise boring.

- any other account is interesting if it has a non-zero balance, or the -E
  flag is used.

-}

module BalanceCommand
where
import Ledger.Utils
import Ledger.Types
import Ledger.Amount
import Ledger.AccountName
import Ledger.Ledger
import Options
import Utils


-- | Print a balance report.
balance :: [Opt] -> [String] -> Ledger -> IO ()
balance opts args l = putStr $ showBalanceReport opts args l

-- | Generate balance report output for a ledger.
showBalanceReport :: [Opt] -> [String] -> Ledger -> String
showBalanceReport opts args l = acctsstr ++ (if collapse then "" else totalstr)
    where 
      acctsstr = concatMap showatree $ subs t
      showatree t = showAccountTreeWithBalances matchedacctnames t
      matchedacctnames = balancereportacctnames l sub apats t
      t = (if empty then id else pruneZeroBalanceLeaves) $ ledgerAccountTree maxdepth l
      apats = fst $ parseAccountDescriptionArgs opts args
      maxdepth = fromMaybe 9999 $ depthFromOpts opts
      sub = SubTotal `elem` opts || (isJust $ depthFromOpts opts)
      empty = Empty `elem` opts
      collapse = Collapse `elem` opts
      totalstr = if isZeroMixedAmount total 
                 then "" 
                 else printf "--------------------\n%s\n" $ padleft 20 $ showMixedAmount total
      total = sum $ map (abalance . ledgerAccount l) $ nonredundantaccts
      nonredundantaccts = filter (not . hasparentshowing) matchedacctnames
      hasparentshowing aname = (parentAccountName $ aname) `elem` matchedacctnames

-- | Identify the accounts we are interested in seeing balances for in the
-- balance report, based on the -s flag and account patterns. See Tests.hs.
balancereportacctnames :: Ledger -> Bool -> [String] -> Tree Account -> [AccountName]
balancereportacctnames l False [] t   = filter (/= "top") $ map aname $ flatten $ treeprune 1 t
balancereportacctnames l False pats t = filter (/= "top") $ ns
    where 
      ns = filter (matchpats_balance pats) $ map aname $ flatten t'
      t' | null $ positivepats pats = treeprune 1 t
         | otherwise = t
balancereportacctnames l True pats t  = nub $ map aname $ addsubaccts l $ as
    where 
      as = map (ledgerAccount l) ns
      ns = balancereportacctnames l False pats t
      -- add (in tree order) any missing subaccounts to a list of accounts
      addsubaccts :: Ledger -> [Account] -> [Account]
      addsubaccts l as = concatMap addsubs as where addsubs = maybe [] flatten . ledgerAccountTreeAt l

-- | Remove all sub-trees whose accounts have a zero balance.
pruneZeroBalanceLeaves :: Tree Account -> Tree Account
pruneZeroBalanceLeaves = treefilter (not . isZeroMixedAmount . abalance)

-- | Show this tree of accounts with balances, eliding boring parent
-- accounts and omitting uninteresting subaccounts based on the provided
-- list of account names we want to see balances for.
showAccountTreeWithBalances :: [AccountName] -> Tree Account -> String
showAccountTreeWithBalances matchednames t = showAccountTreeWithBalances' matchednames 0 "" t
    where
      showAccountTreeWithBalances' :: [AccountName] -> Int -> String -> Tree Account -> String
      showAccountTreeWithBalances' matchednames indent prefix (Node (Account fullname _ bal) subs)
          | isboringparent && hasmatchedsubs = subsprefixed
          | ismatched = this ++ subsindented
          | otherwise = subsnoindent
          where
            subsprefixed = showsubs indent (prefix++leafname++":")
            subsnoindent = showsubs indent ""
            subsindented = showsubs (indent+1) ""
            showsubs i p = concatMap (showAccountTreeWithBalances' matchednames i p) subs
            hasmatchedsubs = any ((`elem` matchednames) . aname) $ concatMap flatten subs
            amt = padleft 20 $ showMixedAmount bal
            this = concatTopPadded [amt, spaces ++ prefix ++ leafname] ++ "\n"
            spaces = "  " ++ replicate (indent * 2) ' '
            leafname = accountLeafName fullname
            ismatched = fullname `elem` matchednames

            -- XXX 
            isboringparent = numsubs >= 1 && (bal == subbal || not ismatched)
            subbal = abalance $ root $ head subs
            numsubs = length subs
            {- gives:
### Failure in: 52:balance report elides zero-balance root account(s)
expected: ""
 but got: "                   0  test\n"
Cases: 58  Tried: 58  Errors: 0  Failures: 1
Eg:
~/src/hledger$ hledger -f sample2.ledger  -s bal
                   0  test
                  $2    a:aa
                 $-2    b
~/src/hledger$ ledger -f sample2.ledger  -s bal
                  $2  test:a:aa
                 $-2  test:b
-}


--          isboringparent = hassubs && (not ismatched || (bal `mixedAmountEquals` subsbal))
--          hassubs = not $ null subs
--          subsbal = sum $ map (abalance . root) subs
            {- gives:
### Failure in: 37:balance report with -s
expected: "                 $-1  assets\n                  $1    bank:saving\n                 $-2    cash\n                  $2  expenses\n                  $1    food\n                  $1    supplies\n                 $-2  income\n                 $-1    gifts\n                 $-1    salary\n                  $1  liabilities:debts\n"
 but got: "                  $1  assets:bank:saving\n                 $-2  assets:cash\n                  $1  expenses:food\n                  $1  expenses:supplies\n                 $-1  income:gifts\n                 $-1  income:salary\n                  $1  liabilities:debts\n"
### Failure in: 39:balance report --depth activates -s
expected: "                 $-1  assets\n                  $1    bank\n                 $-2    cash\n                  $2  expenses\n                  $1    food\n                  $1    supplies\n                 $-2  income\n                 $-1    gifts\n                 $-1    salary\n                  $1  liabilities:debts\n"
 but got: "                  $1  assets:bank\n                 $-2  assets:cash\n                  $1  expenses:food\n                  $1  expenses:supplies\n                 $-1  income:gifts\n                 $-1  income:salary\n                  $1  liabilities:debts\n"
### Failure in: 41:balance report with account pattern o and -s
expected: "                  $1  expenses:food\n                 $-2  income\n                 $-1    gifts\n                 $-1    salary\n--------------------\n                 $-1\n"
 but got: "                  $1  expenses:food\n                 $-1  income:gifts\n                 $-1  income:salary\n--------------------\n                 $-1\n"
### Failure in: 42:balance report with account pattern a
expected: "                 $-1  assets\n                  $1    bank:saving\n                 $-2    cash\n                 $-1  income:salary\n                  $1  liabilities\n--------------------\n                 $-1\n"
 but got: "                  $1  assets:bank:saving\n                 $-2  assets:cash\n                 $-1  income:salary\n                  $1  liabilities\n--------------------\n                 $-1\n"
### Failure in: 43:balance report with account pattern e
expected: "                 $-1  assets\n                  $2  expenses\n                  $1    supplies\n                 $-2  income\n                  $1  liabilities:debts\n"
 but got: "                 $-1  assets\n                  $1  expenses:supplies\n                 $-2  income\n                  $1  liabilities:debts\n"
### Failure in: 49:balance report with -E shows zero-balance accounts
expected: "                 $-1  assets\n                  $1    bank\n                  $0      checking\n                  $1      saving\n                 $-2    cash\n--------------------\n                 $-1\n"
 but got: "                  $0  assets:bank:checking\n                  $1  assets:bank:saving\n                 $-2  assets:cash\n--------------------\n                 $-1\n"
### Failure in: 52:balance report elides zero-balance root account(s)
expected: ""
 but got: "                   0  test\n"
Cases: 58  Tried: 58  Errors: 0  Failures: 7
Eg:
~/src/hledger$ hledger -f sample.ledger  -s bal
                  $1  assets:bank:saving
                 $-2  assets:cash
                  $1  expenses:food
                  $1  expenses:supplies
                 $-1  income:gifts
                 $-1  income:salary
                  $1  liabilities:debts
~/src/hledger$ ledger -f sample.ledger  -s bal
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
                  $2  expenses
                  $1    food
                  $1    supplies
                 $-2  income
                 $-1    gifts
                 $-1    salary
                  $1  liabilities:debts
-}
