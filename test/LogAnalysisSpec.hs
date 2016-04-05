module LogAnalysisSpec
    ( main
    , spec
    ) where

import Log
import LogAnalysis

import Control.Monad.IO.Class (liftIO)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parseMessage" $ do
        it "works" $ do
            parseMessage "E 2 562 help help"
                `shouldBe` LogMessage (Error 2) 562 "help help"

            parseMessage "I 29 la la la"
                `shouldBe` LogMessage Info 29 "la la la"

            parseMessage "This is not in the right format"
                `shouldBe` Unknown "This is not in the right format"

    describe "parse" $ do
        it "works" $ do
            messages <- liftIO $ testParse parse 10 "test/files/error.log"
            messages `shouldBe`
                [ LogMessage Info 5053 "pci_id: con ing!"
                , LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
                , LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
                , LogMessage Info 4076 "verse.'"
                , LogMessage Info 4764 "He trusts to you to set them free,"
                , LogMessage Info 858 "your pocket?' he went on, turning to Alice."
                , LogMessage Info 898 "would be offended again."
                , LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)"
                , LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And"
                , LogMessage Info 3899 "hastily."
                ]

    describe "whatWentWrong" $ do
        it "works" $ do
            messages <- liftIO $ testWhatWentWrong parse whatWentWrong "test/files/error.log"
            messages `shouldBe`
                [ "Mustardwatch opened, please close for proper functioning!"
                , "All backup mustardwatches are busy"
                , "Depletion of mustard stores detected!"
                , "Hard drive failure: insufficient mustard"
                , "Twenty seconds remaining until out-of-mustard condition"
                , "Ten seconds remaining until out-of-mustard condition"
                , "Empty mustard reservoir! Attempting to recover..."
                , "Recovery failed! Initiating shutdown sequence"
                ]
