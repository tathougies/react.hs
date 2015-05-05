{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
module React where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer hiding (listen)


import Data.IORef
import Data.Unique
import Data.Functor
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S

import System.Mem.Weak
import System.IO.Unsafe

type React = IO
newtype Moment a = Moment { runMoment:: StateT (S.Set Unique) (WriterT ([IO ()], [IO()]) IO) a }
    deriving (Monad, Applicative, Functor, MonadState (S.Set Unique), MonadWriter ([IO ()], [IO ()]), MonadIO, MonadFix)

data Event a = Event { _eventRegisterListener :: (a -> Moment ()) -> IO (IO ()) }
data Behavior a = Behavior { _behaviorUpdates :: Event (), _behaviorGetValue :: Moment a }

instance Monoid (Event a) where
    mempty = never
    mappend = merge

instance Functor Event where
    fmap f eb = Event { _eventRegisterListener = \listener -> _eventRegisterListener eb (listener . f) }

instance Functor Behavior where
    fmap f x = pure f <*> x

instance Applicative Behavior where
    pure a = Behavior { _behaviorUpdates = mempty
                      , _behaviorGetValue = return a }
    bab <*> ba = Behavior { _behaviorUpdates = _behaviorUpdates bab <> _behaviorUpdates ba
                          , _behaviorGetValue = do ab <- _behaviorGetValue bab
                                                   a <- _behaviorGetValue ba
                                                   return (ab a) }

newEventRegistration :: React ((a -> Moment ()) -> IO (IO ()), a -> Moment ())
newEventRegistration = do listeners <- newIORef M.empty
                          let registerListener listener = do listenerKey <- newUnique
                                                             modifyIORef listeners $ M.insert listenerKey listener
                                                             return (modifyIORef listeners $ M.delete listenerKey)
                              propagateListeners x = do listeners' <- M.elems <$> liftIO (readIORef listeners)
                                                        mapM_ ($ x) listeners'
                          return (registerListener, propagateListeners)

newEvent :: React (Event a, a -> Moment ())
newEvent = do (registerListener, propagateListeners) <- newEventRegistration
              return (Event registerListener, propagateListeners)

react :: React a -> Moment a
react = liftIO

never :: Event a
never = Event (\_ -> return (return ()))

merge :: Event a -> Event a -> Event a
merge a b = Event (\listener -> do
                     unregisterA <- _eventRegisterListener a listener
                     unregisterB <- _eventRegisterListener b listener
                     return (unregisterA >> unregisterB))

switchE :: Behavior (Event a) -> Event a
switchE be = Event (\listener ->
                    do eInitial <- sync $ sample be
                       unregisterV <- newIORef (return ())
                       unregisterListener <- _eventRegisterListener eInitial listener
                       let switchToNewEvent =
                               do readIORef unregisterV >>= id
                                  eNext <- sync $ sample be
                                  unregisterListener' <- _eventRegisterListener eNext listener
                                  writeIORef unregisterV unregisterListener'
                       unregisterBehaviorListener <- _eventRegisterListener (_behaviorUpdates be) (\() -> tell ([], [switchToNewEvent]))
                       writeIORef unregisterV unregisterListener
                       return (readIORef unregisterV >>= id >> unregisterBehaviorListener))

execute :: Event (Moment a) -> React (Event a)
execute ema = do (registerListener, propagateListeners) <- newEventRegistration
                 unregisterListener <- _eventRegisterListener ema (\ma -> ma >>= propagateListeners)
                 addFinalizer ema unregisterListener
                 return (Event registerListener)

updates :: Behavior a -> React (Event a)
updates ba = do (registerListener, propagateListener) <- newEventRegistration
                let propagate = sync (sample ba >>= propagateListener)
                unregisterListener <- _eventRegisterListener (_behaviorUpdates ba) (\() -> tell ([], [propagate]))
                addFinalizer (_behaviorUpdates ba) unregisterListener
                return (Event registerListener)

(<@) :: Behavior a -> Event b -> Event a
ba <@ eb = Event (\listener -> _eventRegisterListener eb $ \_ -> sample ba >>= listener)

switch :: Behavior (Behavior a) -> Behavior a
switch bb = Behavior { _behaviorUpdates = switchE (_behaviorUpdates <$> bb)
                     , _behaviorGetValue = sample bb >>= sample }

accum :: a -> Event (a -> a) -> React (Event a)
accum initial updaters =
    do cell <- newIORef initial
       (registerListener, propagateListeners) <- newEventRegistration
       let evt = Event registerListener
       unregisterEventListener <- _eventRegisterListener updaters $ \updater ->
                                  do liftIO (modifyIORef cell updater)
                                     cellValue <- liftIO (readIORef cell)
                                     propagateListeners cellValue
       addFinalizer evt unregisterEventListener
       return evt

accumB :: a -> Event (a -> a) -> React (Behavior a)
accumB initial updaters = do ea <- accum initial updaters
                             hold initial ea

sample :: Behavior a -> Moment a
sample = _behaviorGetValue

hold :: a -> Event a -> React (Behavior a)
hold initial updates = do cell <- newIORef initial
                          let behavior = Behavior { _behaviorUpdates = () <$ updates
                                                  , _behaviorGetValue = liftIO (readIORef cell) }
                          unregisterUpdates <- _eventRegisterListener updates (\x -> tell ([writeIORef cell x], []))
                          addFinalizer behavior unregisterUpdates
                          return behavior

calm :: Event a -> React (Event a)
calm evt = do key <- liftIO newUnique
              let evt = Event $ \listener -> _eventRegisterListener evt (calmed listener)
                  calmed listener a =
                      do isMember <- S.member key <$> get
                         if isMember
                         then return () -- This listener has already run...
                         else do modify (S.insert key) -- Otherwise, mark that we have run, and run the listener
                                 listener a
              return evt

spill :: Event [a] -> Event a
spill eas = Event (\listener ->
                   _eventRegisterListener eas $
                   \as -> mapM_ listener as)

sync :: Moment a -> IO a
sync m = do (a, (updateHolds, afterActions)) <- runWriterT (evalStateT (runMoment m) S.empty)
            sequence_ updateHolds
            sequence_ afterActions
            return a

listenToBehavior :: Behavior a -> (a -> Moment ()) -> IO (IO ())
listenToBehavior bb handle = do sync $ do initial <- sample bb
                                          handle initial
                                listen (_behaviorUpdates bb) (\() -> let handle' = sync (sample bb >>= handle)
                                                                     in tell ([],[handle']))

listen :: Event a -> (a -> Moment ()) -> IO (IO ())
listen = _eventRegisterListener
