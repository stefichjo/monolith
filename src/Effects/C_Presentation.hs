module Effects.C_Presentation where
import Effects.C_Infrastructure ( interpretIO )
import Effects.B_Language ( app, appSem )

-- TODO REFACTOR JSON

main :: IO ()
main = app >>= print

mainSem :: IO ()
mainSem = interpretIO appSem >>= print
