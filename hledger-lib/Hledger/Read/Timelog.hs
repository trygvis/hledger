{-# LANGUAGE CPP #-}
{-|

A reader for the timelog file format generated by timeclock.el.

From timeclock.el 2.6:

@
A timelog contains data in the form of a single entry per line.
Each entry has the form:

  CODE YYYY/MM/DD HH:MM:SS [COMMENT]

CODE is one of: b, h, i, o or O.  COMMENT is optional when the code is
i, o or O.  The meanings of the codes are:

  b  Set the current time balance, or \"time debt\".  Useful when
     archiving old log data, when a debt must be carried forward.
     The COMMENT here is the number of seconds of debt.

  h  Set the required working time for the given day.  This must
     be the first entry for that day.  The COMMENT in this case is
     the number of hours in this workday.  Floating point amounts
     are allowed.

  i  Clock in.  The COMMENT in this case should be the name of the
     project worked on.

  o  Clock out.  COMMENT is unnecessary, but can be used to provide
     a description of how the period went, for example.

  O  Final clock out.  Whatever project was being worked on, it is
     now finished.  Useful for creating summary reports.
@

Example:

@
i 2007/03/10 12:26:00 hledger
o 2007/03/10 17:26:02
@

-}

module Hledger.Read.Timelog (
       tests_Timelog,
       parseJournal,
)
where
import Control.Monad.Error (ErrorT(..))
import Text.ParserCombinators.Parsec
import Hledger.Data
import Hledger.Read.Common
import Hledger.Read.Journal hiding (parseJournal)


-- | Parse and post-process a "Journal" from timeclock.el's timelog
-- format, saving the provided file path and the current time, or give an
-- error.
parseJournal :: FilePath -> String -> ErrorT String IO Journal
parseJournal = parseJournalWith timelogFile

timelogFile :: GenParser Char LedgerFileCtx JournalUpdate
timelogFile = do items <- many timelogItem
                 eof
                 return $ liftM (foldr (.) id) $ sequence items
    where 
      -- As all ledger line types can be distinguished by the first
      -- character, excepting transactions versus empty (blank or
      -- comment-only) lines, can use choice w/o try
      timelogItem = choice [ ledgerExclamationDirective
                          , liftM (return . addHistoricalPrice) ledgerHistoricalPrice
                          , ledgerDefaultYear
                          , emptyLine >> return (return id)
                          , liftM (return . addTimeLogEntry)  timelogentry
                          ] <?> "timelog entry, or default year or historical price directive"

-- | Parse a timelog entry.
timelogentry :: GenParser Char LedgerFileCtx TimeLogEntry
timelogentry = do
  code <- oneOf "bhioO"
  many1 spacenonewline
  datetime <- ledgerdatetime
  comment <- optionMaybe (many1 spacenonewline >> liftM2 (++) getParentAccount restofline)
  return $ TimeLogEntry (read [code]) datetime (fromMaybe "" comment)

tests_Timelog = TestList [
 ]
