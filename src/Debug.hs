module Debug where

import qualified Debug.Trace as Trace

debug :: Bool
-- debug = True
debug = False

trace :: String -> a -> a
trace s a = if debug then Trace.trace s a else a

traceM :: Monad m => String -> m ()
traceM s = if debug then Trace.traceM s else return ()

