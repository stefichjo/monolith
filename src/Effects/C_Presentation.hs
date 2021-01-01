module Effects.C_Presentation where
import Effects.C_Infrastructure ( interpretIO )
import Effects.B_Language ( app', appSem, ack, fooUser )

-- TODO JSON (PolysemyCleanArchitecture)

main :: IO ()
main = app' >>= print

mainSem :: IO ()
mainSem = interpretIO appSem >>= print

main' :: IO ()
main' = ack fooUser

-- >>> main'
