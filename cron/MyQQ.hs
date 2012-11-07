module MyQQ (literal) where
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

literal :: QuasiQuoter
literal = QuasiQuoter { quoteExp = stringE }

--literal = QuasiQuoter { quoteExp = stringE, quotePat = undefined, quoteType = undefined, quoteDec = undefined }
