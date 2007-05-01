module Options
where
import System.Console.GetOpt
import System.Directory
import System.Environment (getEnv)
import Data.Maybe (fromMaybe)
    
import Utils


usagehdr       = "Usage: hledger [OPTIONS] "++commands++" [ACCTPATTERNS] [-- DESCPATTERNS]\nOptions:"
commands       = "register|balance"
defaultcmd     = "register"
ledgerFilePath = findFileFromOpts "~/ledger.dat" "LEDGER"

options :: [OptDescr Flag]
options = [
 Option ['f'] ["file"]     (ReqArg File "FILE") "ledger file; - means use standard input",
 Option ['s'] ["showsubs"] (NoArg ShowSubs)     "balance report: show subaccounts", -- register: show subtotals
 Option ['h'] ["help"]     (NoArg Help)         "show this help"
 --Option ['V'] ["version"]  (NoArg Version)      "show version"
 ]

data Flag = 
    File String | 
    ShowSubs |
    Help |
    Version
    deriving (Show,Eq)

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv =
    case getOpt RequireOrder options argv of
      (opts,[],[])   -> return (opts, [defaultcmd])
      (opts,args,[]) -> return (opts, args)
      (_,_,errs)     -> ioError (userError (concat errs ++ usage))

-- testoptions RequireOrder ["foo","-v"]
-- testoptions Permute ["foo","-v"]
-- testoptions (ReturnInOrder Arg) ["foo","-v"]
-- testoptions Permute ["foo","--","-v"]
-- testoptions Permute ["-?o","--name","bar","--na=baz"]
-- testoptions Permute ["--ver","foo"]
testoptions order cmdline = putStr $ 
    case getOpt order options cmdline of
      (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n
      (_,_,errs) -> concat errs ++ usage

usage = usageInfo usagehdr options

-- find a file path from options, an env var or a default value
findFileFromOpts :: FilePath -> String -> [Flag] -> IO String
findFileFromOpts defaultpath envvar opts = do
  envordefault <- getEnv envvar `catch` \_ -> return defaultpath
  paths <- mapM tildeExpand $ [envordefault] ++ (concatMap getfile opts)
  return $ last paths
    where
      getfile (File s) = [s]
      getfile _ = []

tildeExpand              :: FilePath -> IO FilePath
tildeExpand ('~':[])     =  getHomeDirectory
tildeExpand ('~':'/':xs) =  getHomeDirectory >>= return . (++ ('/':xs))
-- -- ~name, requires -fvia-C or ghc 6.8
-- --import System.Posix.User
-- -- tildeExpand ('~':xs)     =  do let (user, path) = span (/= '/') xs
-- --                                pw <- getUserEntryForName user
-- --                                return (homeDirectory pw ++ path)
tildeExpand xs           =  return xs
-- -- courtesy of allberry_b

-- ledger pattern args are 0 or more account patterns optionally followed
-- by -- and 0 or more description patterns
parseLedgerPatternArgs :: [String] -> ([String],[String])
parseLedgerPatternArgs args = 
    case "--" `elem` args of
      True -> ((takeWhile (/= "--") args), tail $ (dropWhile (/= "--") args))
      False -> (args,[])
