module Main where

import Crypto.NaCl.Random

import Control.Monad
import Control.Monad.Trans
import Control.Arrow ((&&&))
import Data.Functor ((<$>))

import Data.Conduit
import Data.Char (isUpper, isAlpha)
import Data.Word
import Data.Bits
import Data.Default

import Numeric.Natural
import Numeric.Natural.Internal (Natural(..), unsafePred)

import System.Environment
import System.Exit
import System.Console.GetOpt
import System.IO

import Text.Regex.Posix ((=~))

import qualified Data.ByteString as B
import qualified Data.Conduit.List as C
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Vector as Vector


-- |Get an infinite stream of random bytes from NaCl
randomByteStream :: MonadIO m => ConduitM i Word8 m ()
randomByteStream = forever $ do
    buffer <- liftIO $ randomBytes 8
    B.foldl (\m x -> m >> yield x) (return ()) buffer

-- |Consume a stream of random bytes and make a stream of random bit
-- vectors
nBitInts :: (Monad m, Num n, Whole n, Bits n) => Int -> ConduitM Word8 n m ()
nBitInts n = let nbytes = (n + 7) `div` 8
                 mask = (1 `shift` n - 1) -- bit vector of n 1s

                 -- Loop over the number of requested bytes, shifting and
                 -- ORing them
                 inner 0 out = yield $ out .&. mask
                 inner m out = do
                     byteM <- await
                     case byteM of
                         Nothing -> return ()
                         Just byte -> inner (m - 1) (shift out 8 .|. fromIntegral byte)

                in forever $ inner nbytes 0

-- |Like a generalized "Natural"; asserts that the wrapped type is positive
newtype Pos a = Pos { unwrapPos :: a } deriving (Read, Show, Eq, Ord, Num, Enum, Real, Bits, Integral)

instance Integral a => Whole (Pos a) where
    toNatural (Pos x) = Natural . toInteger $ x
    unsafePred (Pos x) = Pos $ pred x


uniformsMod :: (Monad m, Whole n, Bits n) => n -> ConduitM Word8 n m ()
uniformsMod n = let nbits = ceiling . logBase 2 . fromIntegral $ n
                    in nBitInts nbits =$= C.filter (< n)

-- |Given a stream of random bytes, creates random (uniform) elements of
-- a vector
choices :: Monad m => Vector.Vector a -> ConduitM Word8 a m ()
choices v = let length = Vector.length v
                in uniformsMod (Pos length) =$= C.map ((v Vector.!) . unwrapPos)

-- |Breaks a stream into non-overlapping subsequences of a given size
chunk :: Monad m => Int -> ConduitM a [a] m ()
chunk n = let inner 0 = return []
              inner m = do
                  r <- await
                  case r of
                      Just x -> (x:) <$> inner (m - 1)
                      Nothing -> return []
            in forever $ inner n >>= yield

data Configuration = Configuration {
    cfgDictionaryPath :: FilePath,
    cfgHelp           :: Bool,
    cfgVerbose        :: Bool,
    cfgSeparator      :: Text.Text,
    cfgWordFilter     :: Text.Text -> Bool,
    cfgNumWords       :: Int,
    cfgNumPasswords   :: Maybe Int
}

instance Default Configuration where
    def = Configuration {
        cfgDictionaryPath = "",
        cfgHelp           = False,
        cfgVerbose        = False,
        cfgSeparator      = " ",
        cfgWordFilter     = const True,
        cfgNumWords       = 4,
        cfgNumPasswords   = Nothing
    }


-- |Conjunction on predicates (a -> Bool). This forms a commutative monoid, with
-- (const True) as the identity.
(&?) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
f &? g = uncurry (&&) . (f &&& g)

-- |Adds the given predicate to the word filter
modifyCfgFilter :: (Text.Text -> Bool) -> Configuration -> Configuration
modifyCfgFilter pred cfg = cfg { cfgWordFilter = (cfgWordFilter cfg) &? pred }

-- |Command line options
options :: [OptDescr (Configuration -> Configuration)]
options = [
    Option ['d'] ["dict"]
        (ReqArg (\s c -> c { cfgDictionaryPath = s }) "PATH")
        "Path to dictionary (default stdin)",

    Option ['h'] ["help", "usage"]
        (NoArg $ \c -> c { cfgHelp = True })
        "Display usage information on stdout",

    Option ['v'] ["verbose"]
        (NoArg $ \c -> c { cfgVerbose = True })
        "Display entropy information on stderr",

    Option ['n'] ["num"]
        (ReqArg (\s c -> c { cfgNumPasswords = Just $ read s }) "NUM")
        "Limit number of generated passwords (default ∞)",

    Option ['l'] ["length"]
        (ReqArg (\s c -> c { cfgNumWords = read s }) "NUM")
        "Number of words in a password (default 4)",

    Option ['s'] ["separator"]
        (ReqArg (\s c -> c { cfgSeparator = Text.pack s }) "STRING")
        "Delimiter used to separate words in a password (default \" \")",

    Option ['c'] ["lowercase"]
        (NoArg . modifyCfgFilter $ Text.all (not . isUpper))
        "Discard upper-case letters",

    Option ['a'] ["alpha"]
        (NoArg . modifyCfgFilter $ Text.all isAlpha)
        "Allow only alphabetical characters",

    Option ['S'] ["singular"]
        (NoArg . modifyCfgFilter $ (/= 's') . Text.last)
        "Discard words that end in s",

    Option ['L'] ["wordlength"]
        (ReqArg (\s -> let l = read s
                        in modifyCfgFilter $ (<= l) . Text.length) "LENGTH")
        "Allow only the words below a certain length",
    
    Option ['r'] ["regex"]
        (ReqArg (\s -> modifyCfgFilter $ (=~ s) . Text.unpack) "REGEX")
        "Allow only words matching the given regular expression."
    ]

-- |Prints usage information
usage :: Handle -> IO ()
usage h = hPutStrLn h . (`usageInfo` options) $
    "Computes a password, as described in http://xkcd.com/936" 

-- |Parses command line arguments
parseOpts :: IO Configuration
parseOpts = do
    parseResult <- getOpt Permute options <$> getArgs
    case parseResult of
        (o, _, [])   -> return . foldl (flip id) def $ o
        (_, _, errs) -> do
            hPutStrLn stderr . concat $ errs
            usage stderr
            exitWith . ExitFailure $ -1

main = do
    config <- parseOpts

    when (cfgHelp config) $ do
        usage stderr
        exitSuccess

    let path = cfgDictionaryPath config

    handle <- if null path
                then return stdin
                else openFile path ReadMode

    passwords <- Vector.fromList . filter (cfgWordFilter config) . Text.lines <$> Text.hGetContents handle
            
    let length = Vector.length passwords

    when (cfgVerbose config) $ do
        hPutStrLn stderr $ "Number of words: " ++ show length
        let entropy = (logBase 2 . fromIntegral $ length) * fromIntegral (cfgNumWords config)
        hPutStrLn stderr $ "Password entropy: " ++ show entropy ++ " bits"

    -- If the user specifies a finite number of passwords, use "isolate" to
    -- just get that many. Otherwise, pass through an infinite stream of
    -- values
    let valve = maybe (forever $ await >>= maybe (return ()) yield) C.isolate $ cfgNumPasswords config

    randomByteStream =$= choices passwords =$= chunk (cfgNumWords config) =$= valve $$
        C.mapM_ $ Text.putStrLn . Text.intercalate (cfgSeparator config)
