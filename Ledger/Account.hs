{-|

An 'Account' stores an account name, all transactions in the account
(excluding any subaccounts), and the total balance (including any
subaccounts).

-}

module Ledger.Account
where
import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.Amount
import Ledger.Entry
import Ledger.RawTransaction
import Ledger.Transaction


instance Show Account where
    show (Account a ts b) = printf "Account %s with %d transactions" a $ length ts

nullacct = Account "" [] nullamt

