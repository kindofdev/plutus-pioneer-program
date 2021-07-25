{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedStrings #-}

module Week04.EnglishAuctionTrace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Plutus.Contract.Trace (InitialDistribution)
-- import qualified Plutus.V1.Ledger.Value as V
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Ada

import Data.Map as Map ( fromList )

import Week04.UpdatedEnglishAuction

-- Token goes to w3 (minimum bid < bid w2 < bid w3 )
{- 
    Final balances
    Wallet 1: 
        {, ""}: 1199983086
    Wallet 2: 
        {, ""}: 999983096
    Wallet 3: 
        {3636, "T"}: 1
        {, ""}: 799983096
-}
scenario1 :: EmulatorTrace ()
scenario1 = do
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    h3 <- activateContractWallet w3 endpoints
    callEndpoint @"start" h1 $ StartParams 
        { spDeadline    = slotToBeginPOSIXTime def 10
        , spMinBid      = 100000000
        , spCurrency    = myTokenSymbol
        , spToken       = myTokenName
        }
    void $ Emulator.waitNSlots 1   

    callEndpoint @"bid" h2 $ BidParams 
        { bpCurrency    = myTokenSymbol
        , bpToken       = myTokenName
        , bpBid         = 100000000
        }
    void $ Emulator.waitNSlots 1   

    callEndpoint @"bid" h3 $ BidParams 
        { bpCurrency    = myTokenSymbol
        , bpToken       = myTokenName
        , bpBid         = 200000000
        }
    void $ Emulator.waitUntilSlot 11  

    callEndpoint @"close" h1 $ CloseParams 
        { cpCurrency    = myTokenSymbol
        , cpToken       = myTokenName
        }
    void $ Emulator.waitNSlots 1  
    Extras.logInfo @String "DONE"

-- Token goes to w2 (w3 bid < minimum bid < w2 bid)
{-
    Final balances
    Wallet 1: 
        {, ""}: 1099983086
    Wallet 2: 
        {, ""}: 899983096
        {3636, "T"}: 1
    Wallet 3: 
        {, ""}: 1000000000
-}
scenario2 :: EmulatorTrace ()
scenario2 = do
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    h3 <- activateContractWallet w3 endpoints
    callEndpoint @"start" h1 $ StartParams 
        { spDeadline    = slotToBeginPOSIXTime def 10for
        , spMinBid      = 100000000
        , spCurrency    = myTokenSymbol
        , spToken       = myTokenName
        }
    void $ Emulator.waitNSlots 1   

    callEndpoint @"bid" h2 $ BidParams 
        { bpCurrency    = myTokenSymbol
        , bpToken       = myTokenName
        , bpBid         = 100000000
        }
    void $ Emulator.waitNSlots 1   

    callEndpoint @"bid" h3 $ BidParams 
        { bpCurrency    = myTokenSymbol
        , bpToken       = myTokenName
        , bpBid         = 20000000   -- bid < mininum bid
        }
    void $ Emulator.waitUntilSlot 11  

    callEndpoint @"close" h1 $ CloseParams 
        { cpCurrency    = myTokenSymbolfor
        , cpToken       = myTokenName
        }
    void $ Emulator.waitNSlots 1  
    Extras.logInfo @String "DONE"

-- Token goes back to w1 (no bids)
{-
    Final balances
    Wallet 1: 
        {3636, "T"}: 1
        {, ""}: 999983086
    Wallet 2: 
        {, ""}: 1000000000
    Wallet 3: 
        {, ""}: 1000000000
-}
scenario3 :: EmulatorTrace ()
scenario3 = do
    h1 <- activateContractWallet w1 endpoints
    callEndpoint @"start" h1 $ StartParams 
        { spDeadline    = slotToBeginPOSIXTime def 10
        , spMinBid      = 100000000
        , spCurrency    = myTokenSymbol
        , spToken       = myTokenName
        }
    void $ Emulator.waitUntilSlot 11  

    callEndpoint @"close" h1 $ CloseParams 
        { cpCurrency    = myTokenSymbol
        , cpToken       = myTokenName
        }
    void $ Emulator.waitNSlots 1  
    Extras.logInfo @String "DONE"    

initialDistribution :: InitialDistribution 
initialDistribution  = Map.fromList [(w1, lovelaceValueOf 1000000000 <> singleton myTokenSymbol myTokenName 1)
                                    ,(w2, lovelaceValueOf 1000000000)
                                    ,(w3, lovelaceValueOf 1000000000)
                                    ] 
<<<<<<< HEAD
<<<<<<< HEAD
=======
"T"
>>>>>>> use utility functions for value creation
=======
>>>>>>> fix typo
w1 :: Wallet 
w1 = Wallet 1

w2 :: Wallet 
w2 = Wallet 2

w3 :: Wallet 
w3 = Wallet 3

myTokenSymbol :: CurrencySymbol 
myTokenSymbol = CurrencySymbol "66"

myTokenName :: TokenName 
myTokenName = TokenName "T"

testScenario :: EmulatorTrace () -> IO () 
testScenario = runEmulatorTraceIO' def (EmulatorConfig $ Left initialDistribution) def 

testScenario1 :: IO ()
testScenario1 = testScenario scenario1

testScenario2 :: IO ()
testScenario2 = testScenario scenario2

testScenario3 :: IO ()
testScenario3 = testScenario scenario3