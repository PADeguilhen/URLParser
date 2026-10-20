module Main (main) where

import Parse ( parseStr )
import URL ( parseURL )

main :: IO ()
main = do
  let tst = "https://almdukd.gzpr/wndulfyfi/sshuscl/pbgfevbi/mkxdpoxabx/kclw?wnttv=ihk&np=zfjv#a"
  putStrLn tst
  case parseStr parseURL tst of
    Left err  -> putStrLn err
    Right url -> putStrLn (show url)
