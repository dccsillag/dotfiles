{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Csillag.TagSystem
  ( viewTag
  , hideTag
  , shiftTag
  , renameTag
  , newTag
  , dropTag
  , tagSystem
  )
where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Util.ExtensibleState   as XS


type TagId = String

data TagState = TagState { tags :: Map String [Window]
                         , active :: Map WorkspaceId [String]
                         }
              deriving Typeable


viewTag :: TagId -> X ()
viewTag tagid = do
  modify
    (\st ->
      st { active = Map.insert (W.tag $ W.workspace $ W.current ws) $ active st
         } :: TagState -> TagState
    )

hideTag :: TagId -> X ()
hideTag tagid = do
  modify
    (\st ->
      st
        { active = Map.adjust (filter (/= tagid))
                              (W.tag $ W.workspace $ W.current ws)
                              (active st)
        } :: TagState -> TagState
    )

shiftTag :: Window -> TagId -> X ()
shiftTag win tagid = do
    modify
      (\st ->
       st
         { tags = Map.adjust (win:) tagid $ Map.adjust (filter (/=win)) curtag $ tags st
         }
      )
    where curtag = undefined

renameTag :: TagId -> TagId -> X ()
renameTag tagid tagid' = st
  { active = Map.adjust (map $ \x -> if x == tagid then tagid' else x)
                        (W.tag $ W.workspace $ W.current ws)
                        (active st)
  }

newTag :: TagId -> X ()
newTag tagid = st { tags = Map.insert tagid [] $ tags st }

dropTag :: TagId -> X ()
dropTag tagid | tagid `elem` active st = st
              | otherwise = st { tags = Map.delete tagid $ tags st }

tagSystem :: Int -> XConfig l -> XConfig l
tagSystem n_workspaces xconfig = xconfig
  { startupHook = startupHook xconfig
  , workspaces  = ('t' :) . show <$> [1 .. n_workspaces]
  }
