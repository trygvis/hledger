{-|

A 'RawLedger' is a parsed ledger file. We call it raw to distinguish from
the cached 'Ledger'.

-}

module Ledger.RawLedger
where
import qualified Data.Map as Map

import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.Entry


instance Show RawLedger where
    show l = printf "RawLedger with %d entries"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))