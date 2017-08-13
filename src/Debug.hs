{-# language CPP #-}

module Debug where

import Debug.Trace

dbTrace :: a -> String -> a
#ifdef DEBUG
dbTrace a t = trace t a
#else
dbTrace a _ = a
#endif

dbTraceM :: Monad m => String -> m ()
#ifdef DEBUG
dbTraceM t = traceM t
#else
dbTraceM _ = return ()
#endif

