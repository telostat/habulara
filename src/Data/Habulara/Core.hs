-- | The basic idea of Habulara is to take a raw CSV row record and map it into
-- a Habulara CSV row record:
--
--
-- > ┌────────────────┐             ┌─────────────────────┐
-- > │ Raw CSV Record │──── map ───>│ Habulara CSV Record │
-- > └────────────────┘             └─────────────────────┘
--
--
-- Essential types are given below.
module Data.Habulara.Core (
  module Types,
) where

import Data.Habulara.Core.Types as Types

