{-# language CPP #-}

module Debug where

import qualified Debug.Trace as Trace

trace :: String -> a -> a
#ifdef DEBUG
trace s a = Trace.trace s a
#else
trace a _ = a
#endif

traceM :: Monad m => String -> m ()
#ifdef DEBUG
traceM s = Trace.traceM s
#else
traceM _ = return ()
#endif

