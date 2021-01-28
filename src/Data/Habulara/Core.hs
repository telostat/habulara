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

module Data.Habulara.Core
  ( module Record
  , module Value
  , module Class
  , module NonEmpty
  ) where


import Data.Habulara.Core.Class    as Class
import Data.Habulara.Core.NonEmpty as NonEmpty
import Data.Habulara.Core.Record   as Record
import Data.Habulara.Core.Value    as Value
