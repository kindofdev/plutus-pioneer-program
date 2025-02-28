{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week07.TestRockPaperScissors where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Ledger
import           Ledger.TimeSlot
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..))
import           Wallet.Emulator.Wallet

import           Week07.RockPaperScissors

test :: IO ()
test = do
    test'  Rock     Rock
    test'  Rock     Paper
    test'  Rock     Scissors
    test'  Scissors Rock
    test'  Scissors Paper
    test'  Scissors Scissors
    test'  Paper    Rock
    test'  Paper    Paper
    test'  Paper    Scissors
    test'' Rock 

test' :: GameChoice -> GameChoice -> IO ()
test' c1 c2 = runEmulatorTraceIO $ myTrace c1 c2

test'' :: GameChoice -> IO ()
test'' = runEmulatorTraceIO . myTrace'

myTrace :: GameChoice -> GameChoice -> EmulatorTrace ()
myTrace c1 c2 = do
    Extras.logInfo $ "first move: " ++ show c1 ++ ", second move: " ++ show c2

    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
        pkh2      = pubKeyHash $ walletPubKey $ Wallet 2
        stake     = 5_000_000
        deadline1 = slotToEndPOSIXTime def 5
        deadline2 = slotToEndPOSIXTime def 10

        fp = FirstParams
                { fpSecond         = pkh2
                , fpStake          = stake
                , fpPlayDeadline   = deadline1
                , fpRevealDeadline = deadline2
                , fpNonce          = "SECRETNONCE"
                , fpChoice         = c1
                }

    callEndpoint @"first" h1 fp

    tt <- getTT h1

    let sp = SecondParams
                { spFirst          = pkh1
                , spStake          = stake
                , spPlayDeadline   = deadline1
                , spRevealDeadline = deadline2
                , spChoice         = c2
                , spToken          = tt
                }

    void $ Emulator.waitNSlots 3

    callEndpoint @"second" h2 sp

    void $ Emulator.waitNSlots 10
  where
    getTT :: ContractHandle (Last ThreadToken) GameSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt


myTrace' :: GameChoice -> EmulatorTrace ()
myTrace' c1 = do
    Extras.logInfo $ "first and unique move: " ++ show c1 

    h1 <- activateContractWallet (Wallet 1) endpoints

    let pkh2      = pubKeyHash $ walletPubKey $ Wallet 2
        stake     = 5_000_000
        deadline1 = slotToEndPOSIXTime def 5
        deadline2 = slotToEndPOSIXTime def 10

        fp = FirstParams
                { fpSecond         = pkh2
                , fpStake          = stake
                , fpPlayDeadline   = deadline1
                , fpRevealDeadline = deadline2
                , fpNonce          = "SECRETNONCE"
                , fpChoice         = c1
                }

    callEndpoint @"first" h1 fp
    void $ Emulator.waitNSlots 10