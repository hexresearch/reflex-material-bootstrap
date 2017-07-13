module Web.Reflex.Bootstrap.Context(
    ComponentContext(..)
  ) where

import GHC.Generics

-- | Supported bootstrap color context
data ComponentContext =
    NoContext
  | InfoContext
  | SuccessContext
  | WarningContext
  | DangerContext
  deriving (Eq, Show, Read, Enum, Bounded, Generic)
