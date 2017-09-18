module Debug where

import qualified Debug.Trace as Trace

debug :: Bool
debug = True

trace :: String -> a -> a
trace s a = if debug then Trace.trace s a else a

traceShowId :: Show a => a -> a
traceShowId = if debug then Trace.traceShowId else id

traceAppend :: Show a => String -> a -> a
traceAppend s a = trace (s ++ show a) a

traceM :: Monad m => String -> m ()
traceM s = if debug then Trace.traceM s else return ()

