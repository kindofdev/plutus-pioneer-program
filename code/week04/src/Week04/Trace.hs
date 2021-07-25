{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s

happypathTrace :: EmulatorTrace ()
happypathTrace = do
    let (w1, w2, w3) = (Wallet 1, Wallet 2, Wallet 3)
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    h3 <- activateContractWallet w3 endpoints
    callEndpoint @"give" h1 $ GiveParams 
        { gpBeneficiary = pubKeyHash $ walletPubKey w2
        , gpDeadline    = slotToBeginPOSIXTime def 10
        , gpAmount      = 10000000
        }
    void $ Emulator.waitNSlots 1    

    callEndpoint @"give" h1 $ GiveParams 
        { gpBeneficiary = pubKeyHash $ walletPubKey w2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }    
    void $ Emulator.waitNSlots 1 
    
    callEndpoint @"give" h1 $ GiveParams 
        { gpBeneficiary = pubKeyHash $ walletPubKey w3
        , gpDeadline    = slotToBeginPOSIXTime def 10
        , gpAmount      = 10000000
        }    
    void $ Emulator.waitUntilSlot 10
    callEndpoint @"grab" h3 ()   
    void $ Emulator.waitUntilSlot 20
    callEndpoint @"grab" h2 ()   
    void $ Emulator.waitNSlots 1
    Extras.logInfo @String "DONE"

testHappypath :: IO ()
testHappypath = runEmulatorTraceIO happypathTrace