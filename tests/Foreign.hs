module Foreign(main) where
import Prelude
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Control.Monad

type Idx = Int

foreign import ccall "uv_loop_init" uv_loop_init :: IO Int
foreign import ccall "uv_tcp_init" uv_tcp_init :: Idx -> IO Int
foreign import ccall "uv_tcp_bind" uv_tcp_bind :: Idx -> IO Int
foreign import ccall "uv_listen" uv_listen :: Idx -> Int -> IO Int
foreign import ccall "uv_run_once" uv_run_once :: IO Int
foreign import ccall "janus_get_nread" janus_get_nread :: Idx -> IO Int
foreign import ccall "janus_get_data" janus_get_data :: Idx -> IO CString
foreign import ccall "janus_get_triggered" janus_get_triggered :: Idx -> IO Int
foreign import ccall "janus_get_failed" janus_get_failed :: Idx -> IO Int
foreign import ccall "janus_write" janus_write :: Idx -> CString -> IO Int

main :: IO ()
main = do
  suc <- uv_loop_init
  when (suc /= 0) (fail "no loop_init")
  suc <- uv_tcp_init 0
  when (suc /= 0) (fail "no tcp_init")
  suc <- uv_tcp_bind 0
  when (suc /= 0) (fail "no tcp_bind")
  putStrLn "running listen"
  suc <- uv_listen
    0 -- idx
    1 -- backlog
  when (suc /= 0) (fail "no listen")
  let
    loop = do
      suc <- uv_run_once
      if suc == 0
        then do
          -- uv docs:
          -- "no active handles or requests left"
          putStrLn "done"
        else do
          triggered <- janus_get_triggered 0
          putStrLn "triggered"
          print triggered

          if triggered == 0
            then loop
            else do
              failed <- janus_get_failed 0
              putStrLn "failed"
              print failed
              if failed == 1
                then putStrLn "quitting because conn failed"
                else do
                  nread <- janus_get_nread 0
                  putStrLn "nread"
                  print nread

                  readBytesPtr <- janus_get_data 0
                  received <- peekCAStringLen (readBytesPtr, nread)
                  print received

                  withCAString received $ \cstring ->
                    janus_write 0 cstring

                  putStrLn "looping"
                  loop
  loop
