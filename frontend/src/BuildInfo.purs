module CE2.BuildInfo (buildStamp, isStaticDeploy) where

foreign import buildStamp :: String
foreign import isStaticDeploy :: Boolean
