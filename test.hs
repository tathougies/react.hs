module Main where

import Control.Applicative

import Control.Concurrent
import Control.Monad.Trans
import Control.Monad

import Data.Monoid

import React

main :: IO ()
main = do (a, sendA) <- newEvent
          (b, sendB) <- newEvent
          ba <- hold (0 :: Int) a
          bb <- hold (0 :: Int) b

          let bab = (+) <$> ba <*> bb

          bsum <- accumB 0 (((+) <$> a) <> ((+) <$> b))

          listenToBehavior bab (\new -> liftIO (putStrLn ("Value is now " ++ show new)))
          listenToBehavior bsum (\new -> liftIO (putStrLn ("Sum is now " ++ show new)))
          forever $ do x <- getLine
                       sync $ case x of
                                ('a':xs) -> sendA (read xs)
                                ('b':xs) -> sendB (read xs)
                                _ -> return ()

