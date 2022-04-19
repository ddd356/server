import qualified Control.Concurrent.Async as Async (withAsync, cancel)
import qualified Test.QuickCheck as Q (quickCheck, quickCheckWith, Args(..), stdArgs, Property(..))
import qualified Test.QuickCheck.Monadic as QM (monadicIO, run, assert)
import qualified Network.Wai.Handler.Warp as W (run)
import qualified App as A (app)
import qualified Control.Exception.Base as EB (catch)
import qualified Control.Exception as E (SomeException(..))

prop_CreateTestDatabase :: Q.Property
prop_CreateTestDatabase = QM.monadicIO $ do
        result <- QM.run $ EB.catch ( return True ) ( \e -> return False  )
        QM.assert result
--(e :: E.SomeException)

prop_RemoveTestDatabase :: Bool
prop_RemoveTestDatabase = True

main :: IO ()
main = do
    Async.withAsync (W.run 8080 A.app) $ \a1 ->
        do
            Q.quickCheckWith a prop_CreateTestDatabase 
            Q.quickCheckWith a prop_RemoveTestDatabase
            Async.cancel a1
            return () where
                a = Q.Args {
                    Q.replay = Q.replay Q.stdArgs,
                    Q.maxSuccess = 1,
                    Q.maxDiscardRatio = Q.maxDiscardRatio Q.stdArgs,
                    Q.maxSize = Q.maxSize Q.stdArgs,
                    Q.chatty = Q.chatty Q.stdArgs,
                    Q.maxShrinks = Q.maxShrinks Q.stdArgs
                    }
