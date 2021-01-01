module Effects.C_Presentation where
import Effects.C_Infrastructure ( interpretIO )
import Effects.B_Language ( app, app', appSem, ack, fooUser, fooCommand, Event )

-- TODO JSON (PolysemyCleanArchitecture)

main :: IO ()
main = app' >>= print

mainSem :: IO ()
mainSem = interpretIO appSem >>= print

main' :: IO ()
main' = ack fooUser

main'' :: IO Event
main'' = app fooCommand -- >>= ack

-- >>> main''
-- out/db: openFile: resource busy (file is locked)
