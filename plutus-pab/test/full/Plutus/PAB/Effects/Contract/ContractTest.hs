{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-

"inline" contracts from plutus-use-cases for testing

-}
module Plutus.PAB.Effects.Contract.ContractTest(
    TestContracts(..)
    , handleContractTest
    ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Bifunctor                      (Bifunctor (..))
import           Data.Row
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                        (Generic)

import qualified ContractExample.AtomicSwap          as Contracts.AtomicSwap
import qualified ContractExample.PayToWallet         as Contracts.PayToWallet
import           Data.Text.Extras                    (tshow)
import qualified Plutus.Contracts.Currency           as Contracts.Currency
import qualified Plutus.Contracts.GameStateMachine   as Contracts.GameStateMachine
import qualified Plutus.Contracts.PingPong           as Contracts.PingPong
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinContract (..), BuiltinHandler, SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Types                    (PABError (..))

data TestContracts = GameStateMachine | Currency | AtomicSwap | PayToWallet | PingPong
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty TestContracts where
    pretty = viaShow

-- | A mock/test handler for 'ContractEffect'. Uses 'Plutus.PAB.Effects.Contract.Builtin'.
handleContractTest :: BuiltinHandler TestContracts
handleContractTest = Builtin.handleBuiltin

instance BuiltinContract TestContracts where
  schema = \case
    GameStateMachine -> Builtin.endpointsToSchemas @Contracts.GameStateMachine.GameStateMachineSchema
    Currency         -> Builtin.endpointsToSchemas @Contracts.Currency.CurrencySchema
    AtomicSwap       -> Builtin.endpointsToSchemas @Contracts.AtomicSwap.AtomicSwapSchema
    PayToWallet      -> Builtin.endpointsToSchemas @Contracts.PayToWallet.PayToWalletSchema
    PingPong         -> Builtin.endpointsToSchemas @Contracts.PingPong.PingPongSchema

  contractDefinition = \case
    GameStateMachine -> SomeBuiltin Contracts.GameStateMachine.contract
    Currency         -> SomeBuiltin Contracts.Currency.mintCurrency
    AtomicSwap       -> SomeBuiltin $ first tshow Contracts.AtomicSwap.atomicSwap
    PayToWallet      -> SomeBuiltin Contracts.PayToWallet.payToWallet
    PingPong         -> SomeBuiltin Contracts.PingPong.combined
