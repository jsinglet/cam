module QQ(qq) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote

qq = QuasiQuoter { quoteExp = stringE }
 
