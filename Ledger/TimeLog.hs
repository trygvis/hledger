{-|

A 'TimeLog' is a parsed timelog file (generated by timeclock.el).
It contains zero or more 'TimeLogEntry's.

-}

module Ledger.TimeLog
where
import Ledger.Utils
import Ledger.Types
import Ledger.Currency
import Ledger.Amount
import Ledger.RawTransaction
import Ledger.Entry
import Ledger.RawLedger

instance Show TimeLogEntry where 
    show t = printf "%s %s %s" (show $ tlcode t) (tldatetime t) (tlcomment t)

instance Show TimeLog where
    show tl = printf "TimeLog with %d entries" $ length $ timelog_entries tl

ledgerFromTimeLog :: TimeLog -> RawLedger
ledgerFromTimeLog tl = 
    RawLedger [] [] (entriesFromTimeLogEntries $ timelog_entries tl) ""

entriesFromTimeLogEntries :: [TimeLogEntry] -> [Entry]

entriesFromTimeLogEntries [clockin] = 
    entriesFromTimeLogEntries [clockin, clockoutNowEntry]

entriesFromTimeLogEntries [clockin,clockout] =
    [
     Entry {
       edate         = indate,
       estatus       = True,
       ecode         = "",
       edescription  = accountname,
       ecomment      = "",
       etransactions = [
        RawTransaction accountname amount "",
        RawTransaction "TIME" (-amount) ""
       ],
       epreceding_comment_lines=""}
    ]
    where
      accountname = tlcomment clockin
      intime      = tldatetime clockin
      indate      = dateFrom $ tldatetime clockin
      outtime     = tldatetime clockout
      amount      = hours 0 -- read $ outtime - intime

entriesFromTimeLogEntries many =
    (entriesFromTimeLogEntries $ take 2 many) ++
    (entriesFromTimeLogEntries $ drop 2 many)

clockoutNowEntry = TimeLogEntry ' ' "" ""
dateFrom = id