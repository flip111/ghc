{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module GHC.Hs.PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.Pat where

import Outputable
import GHC.Hs.Extension ( OutputableBndrId, GhcPass, XRec )
import Data.Kind

type role Pat nominal
data Pat (i :: Type)
type LPat i = XRec i Pat

instance OutputableBndrId p => Outputable (Pat (GhcPass p))
