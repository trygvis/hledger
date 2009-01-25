{-| 

A happs-based web UI for hledger.

-}

module WebCommand
where
import qualified Data.Map as Map
import Data.Map ((!))
import HAppS.Server
import Ledger
import Options
import BalanceCommand
import RegisterCommand
import PrintCommand


tcpport = 5000

web :: [Opt] -> [String] -> Ledger -> IO ()
web opts args l = do
  putStrLn $ printf "starting hledger web server on port %d" tcpport
  simpleHTTP nullConf{port=tcpport} [
                     method GET $ ok $ toResponse $ output
                    ]
      where output = showBalanceReport (opts++[SubTotal]) [] l
