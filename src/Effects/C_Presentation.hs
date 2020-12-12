module Effects.C_Presentation where
import Effects.C_Infrastructure
import Effects.B_Language

-- TODO REFACTOR JSON

main :: IO ()
main = app >>= print

mainSem :: IO ()
mainSem = runSemIO appSem >>= print
