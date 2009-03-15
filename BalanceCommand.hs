{-| 

A ledger-compatible @balance@ command. 

ledger's balance command is easy to use but hard to describe precisely.
Here are some attempts.


I. high level description with examples
---------------------------------------

We'll use sample.ledger, which has the following account tree:

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
Subaccounts are displayed with more indentation. Each balance is the sum
of any transactions in that account plus any balances from subaccounts:

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

Usually, the non-interesting accounts are elided or omitted. To be
precise, an interesting account is one with: a non-zero balance, or a
balance different from its single subaccount, or two or more interesting
subaccounts. (More subtleties to be filled in here.)

So, above, @checking@ is omitted because it has no interesting subaccounts
and a zero balance. @bank@ is elided because it has only a single
interesting subaccount (saving) and it would be showing the same balance
($1). Ditto for @liabilities@.

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


II. Some notes for the implementation
-------------------------------------

- a simple balance report shows top-level accounts

- with an account pattern, it shows accounts whose leafname matches, plus their parents

- with the subtotal option, it also shows all subaccounts of the above

- zero-balance leaf accounts are removed

- the resulting account tree is displayed with each account's aggregated
  balance, with boring parents prefixed to the next line

- a boring parent has the same balance as its child and is not explicitly
  matched by the display options.

- the sum of the balances shown is displayed at the end, if it is non-zero


III. John's description 2009/02
-------------------------------

johnw: \"Since you've been re-implementing the balance report in Haskell, I thought
I'd share with you in pseudocode how I rewrote it for Ledger 3.0, since
the old method never stopped with the bugs.  The new scheme uses a 5 stage
algorithm, with each stage gathering information for the next:

STEP 1

Based on the user's query, walk through all the transactions in their
journal, finding which ones to include in the account subtotals. For each
transaction that matches, mark the account as VISITED.

STEP 2

Recursively walk the accounts tree, depth-first, computing aggregate
totals and aggregate \"counts\" (number of transactions contributing to the
aggregate total).

STEP 3

Walk the account tree again, breadth-first, and for every VISITED account,
check whether it matches the user's \"display predicate\".  If so, mark the
account as MATCHING.

STEP 4

Do an in-order traversal of the account tree.  Except for the top-most
account (which serves strictly as a container for the other accounts):

a. If the account was MATCHING, or two or more of its children are
   MATCHING or had descendents who were MATCHING, display the account.

b. Otherwise, if the account had *any* children or descendants who
   were VISITED and *no* children or descendants who were MATCHING,
   then apply the display predicate from STEP 3 to the account.  If
   it matches, also print this account.  (This step allows -E to
   report empty accounts which otherwise did match the original
   query).

STEP 5

When printing an account, display a \"depth spacer\" followed by the \"partial name\".
tal
The partial name is found by taking the base account's name, then
prepending to it every non-MATCHING parent until a MATCHING parent is
found.

The depth spacer is found by outputting two spaces for every MATCHING parent.

This way, \"Assets:Bank:Checking\" might be reported as:

 Assets
   Bank
     Checking

or

 Assets
   Bank:Checking

or

 Assets:Bank:Checking

Depending on whether the parents were or were not reported for their own reasons.
\"

\"I just had to add one more set of tree traversals, to correctly determine
whether a final balance should be displayed

without --flat, sort at each level in the hierarchy
with --flat, sort across all accounts\"

IV. A functional description
-----------------------------

1. filter the transactions, keeping only those included in the calculation.
   Remember the subset of accounts involved. (VISITED)

2. generate a full account & balance tree from all transactions

3. Remember the subset of VISITED accounts which are matched for display.
   (MATCHING)

4. walk through the account tree:

   a. If the account is in MATCHING, or two or more of its children are or
      have descendants who are, display it.

   b. Otherwise, if the account has any children or descendants in VISITED
      but none in MATCHING, and it is matched for display, display it.
      (allows -E to report empty accounts which otherwise did match the
      original query).

5. when printing an account, display a \"depth spacer\" followed by the
   \"partial name\".  The partial name is found by taking the base account's
   name, then prepending to it every non-MATCHING parent until a MATCHING
   parent is found. The depth spacer is two spaces per MATCHING parent.

6. I just had to add one more set of tree traversals, to correctly
   determine whether a final balance should be displayed

7. without --flat, sort at each level in the hierarchy
   with --flat, sort across all accounts

V. Another functional description with new terminology
------------------------------------------------------

- included transactions are those included in the calculation, specified
  by -b, -e, -p, -C, -R, account patterns and description patterns.

- included accounts are the accounts referenced by included transactions.

- matched transactions are the included transactions which match the
  display predicate, specified by -d.

- matched accounts are the included accounts which match the display
  predicate, specified by -d, --depth, -E, -s

- an account name tree is the full hierarchy of account names implied by a
  set of transactions

- an account tree is an account name tree augmented with the aggregate
  balances and transaction counts for each named account

- the included account tree is the account tree for the included transactions

- a matched account tree contains one or more matched accounts

- to generate the balance report, walk through the included account tree
  and display each account if

  - it is matching

  - or it has two more more matching subtrees

  - or it has included offspring but no matching offspring

- to display an account, display an indent then the \"partial name\".  The
  partial name is the account's name, prefixed by each unmatched parent
  until a matched parent is found. The indent is two spaces per matched
  parent.


VI. John's description 2009/03/11
---------------------------------

johnw: \"Well, I had to rewrite the balance reporting code yet again, 
because it wouldn't work with --depth correctly.  Here's the new algorithm.

STEP 1: Walk all postings, looking for those that match the user's query.
       As a match is found, mark its account VISITED.

STEP 2: Do a traversal of all accounts, sorting as need be, and collect
       them all into an ordered list.

STEP 3: Keeping that list on the side, do a *depth-first* traversal of
       the account tree.

       visited    = 0
       to_display = 0

       (visited, to_display) += <recurse for all immediate children>

       if account is VISITED or (no --flat and visited > 0):
           if account matches display predicate and
              (--flat or to_display != 1):
               mark account as TO_DISPLAY
               to_display = 1
           visited = 1

       return (visited, to_display)

STEP 4: top_displayed = 0

       for every account in the ordered list:
           if account has been marked TO_DISPLAY:
               mark account as DISPLAYED
               format the account and print

           if --flat and account is DISPLAYED:
               top_displayed += 1

       if no --flat:
           for every top-most account:
               if account is DISPLAYED or any children or DISPLAYED:
                   top_displayed += 1

       if no --no-total and top_displayed > 1 and
          top-most accounts sum to a non-zero balance:
           output separator
           output balance sum account as DISPLAYED
               format the account and print

           if --flat and account is DISPLAYED:
               top_displayed += 1

       if no --flat:
           for every top-most account:
               if account is DISPLAYED or any children or DISPLAYED:
                   top_displayed += 1

       if no --no-total and top_displayed > 1 and
          top-most accounts sum to a non-zero balance:
           output separator
           output balance sum
\"


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
