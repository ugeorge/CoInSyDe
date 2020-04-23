module  CoInSyDe.Target where

import Data.Proxy
import Data.Text (unpack)

import CoInSyDe.Frontend
import CoInSyDe.Target.C.Frontend
import CoInSyDe.Internal.Config

getCProxies = ( undefined :: Proxy CType
              , undefined :: Proxy (Nv CRequ CPort)
              , undefined :: Proxy (Tm CRequ CPort)
              , undefined :: Proxy (Pt CRequ CPort)
              )
