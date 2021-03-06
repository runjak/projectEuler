module Main where

import Control.Monad (forM_)
import qualified Problem1
import qualified Problem2
import qualified Problem3
import qualified Problem4
import qualified Problem5
import qualified Problem6
import qualified Problem7
import qualified Problem8
import qualified Problem9
import qualified Problem10
import qualified Problem11
import qualified Problem12
import qualified Problem13
import qualified Problem14
import qualified Problem15
import qualified Problem16
import qualified Problem17
import qualified Problem18
import qualified Problem19
import qualified Problem20
import qualified Problem21
import qualified Problem22
import qualified Problem23
import qualified Problem24
import qualified Problem25
import qualified Problem26
import qualified Problem27
import qualified Problem28
import qualified Problem29
import qualified Problem30
import qualified Problem31
import qualified Problem32
import qualified Problem33
import qualified Problem34
import qualified Problem35
import qualified Problem36
import qualified Problem37
import qualified Problem38
import qualified Problem39
import qualified Problem40
import qualified Problem41
import qualified Problem42
import qualified Problem43
import qualified Problem44
import qualified Problem45
import qualified Problem46
import qualified Problem47
import qualified Problem48
import qualified Problem49
import qualified Problem50
import qualified Problem52
import qualified Problem55
import qualified Problem57
import qualified Problem58
import qualified Problem59
import qualified Problem65
import qualified Problem79
import qualified Problem89
import qualified Problem92
import qualified Problem96
import qualified Problem148
import qualified Problem187
import qualified Problem215

problems :: [(String, IO ())]
problems = [("Problem1: ", Problem1.main),
            ("Problem2: ", Problem2.main),
            ("Problem3: ", Problem3.main),
            ("Problem4: ", Problem4.main),
            ("Problem5: ", Problem5.main),
            ("Problem6: ", Problem6.main),
            ("Problem7: ", Problem7.main),
            ("Problem8: ", Problem8.main),
            ("Problem9: ", Problem9.main),
            ("Problem10: ", Problem10.main),
            ("Problem11: ", Problem11.main),
            ("Problem12: ", Problem12.main),
            ("Problem13: ", Problem13.main),
            ("Problem14: ", Problem14.main),
            ("Problem15: ", Problem15.main),
            ("Problem16: ", Problem16.main),
            ("Problem17: ", Problem17.main),
            ("Problem18: ", Problem18.main),
            ("Problem19: ", Problem19.main),
            ("Problem20: ", Problem20.main),
            ("Problem21: ", Problem21.main),
            ("Problem22: ", Problem22.main),
            ("Problem23: ", Problem23.main),
            ("Problem24: ", Problem24.main),
            ("Problem25: ", Problem25.main),
            ("Problem26: ", Problem26.main),
            ("Problem27: ", Problem27.main),
            ("Problem28: ", Problem28.main),
            ("Problem29: ", Problem29.main),
            ("Problem30: ", Problem30.main),
            ("Problem31: ", Problem31.main),
            ("Problem32: ", Problem32.main),
            ("Problem33: ", Problem33.main),
            ("Problem34: ", Problem34.main),
            ("Problem35: ", Problem35.main),
            ("Problem36: ", Problem36.main),
            ("Problem37: ", Problem37.main),
            ("Problem38: ", Problem38.main),
            ("Problem39: ", Problem39.main),
            ("Problem40: ", Problem40.main),
            ("Problem41: ", Problem41.main),
            ("Problem42: ", Problem42.main),
            ("Problem43: ", Problem43.main),
            ("Problem44: ", Problem44.main),
            ("Problem45: ", Problem45.main),
            ("Problem46: ", Problem46.main),
            ("Problem47: ", Problem47.main),
            ("Problem48: ", Problem48.main),
            ("Problem49: ", Problem49.main),
            ("Problem50: ", Problem50.main),
            ("Problem52: ", Problem52.main),
            ("Problem55: ", Problem55.main),
            ("Problem57: ", Problem57.main),
            ("Problem58: ", Problem58.main),
            ("Problem59: ", Problem59.main),
            ("Problem65: ", Problem65.main),
            ("Problem79: ", Problem79.main),
            ("Problem89: ", Problem89.main),
            ("Problem92: ", Problem92.main),
            ("Problem96: ", Problem96.main),
            ("Problem148: ", Problem148.main),
            ("Problem187: ", Problem187.main),
            ("Problem215: ", Problem215.main)]

main = forM_ problems $ \(desc, p) -> putStr desc >> p
