{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import qualified Duden
import qualified DWDS
import qualified Types

import Types (AnkiNote (..), Wort (..), Mp3Url (..), SearchResult (..))

main :: IO ()
main = hspec $ do
    describe "Types.extractWord" $ do
        it "should extract the word correctly" $ do
            let mkTest (flds, expecteWort) =
                  Types.extractWord (AnkiNote 1 flds "") `shouldBe` (Wort expecteWort)
            mapM_ mkTest
                [ ("pralinka\USe Praline (-, -n)\US\USy", "Praline")
                , ("vydání (knihy ap.)\USe Auflage (-, -n)\US\USy", "Auflage")
                , ("zálusk, choutky (na co)\USs Gelüste (auf 4.p)\US\USy", "Gelüste")
                , ("připravit se na něco (2 předložky)\US(sich) vor/bereiten (auf etw 4.p / für etw)\US\USy", "vorbereiten")
                , ("trpět (čím)<br />(min?)\USleiden (unter etw) - litt - hat gelitten \US\USy", "leiden")
                , ("zadržet, zastavit<br />(min?)<br />(er?)\USan/halten - hielt an - hat angehalten<br />er hält an\US\USy", "anhalten")
                , ("zemřít (na něco)<br />(er?)<br />(min?)\USsterben (an etw - 3.p!) - starb - ist gestorben<br />er stirbt\US\USy", "sterben")
                , ("napadnou (někoho) (o myšlence)<br />(min?)<br />(er?)\USein/fallen (j-m) (3.p!) - fiel ein - ist eingefallen<br />fällt ein\USDann <b>fiel</b> ihnen Frederick <b>ein</b>.<br>Es ist uns leider nichts <b>eingefallen</b>.\USy", "einfallen")
                , ("prosit (někoho o něco)<br />(min?)\USbitten (j-n um etw) - bat - hat gebeten\US\USy", "bitten")
                ]

    describe "DWDS.getMp3Url" $ do
        it "should retrieve URL of pron mp3" $ do
            DWDS.getMp3Url (Wort "Bär") `shouldReturn` PronFound (Mp3Url "http://media.dwds.de/dwds2/audio/005/der_Baer.mp3")
        it "should return PronNotAvailable when word has no pron" $ do
            DWDS.getMp3Url (Wort "Ärger") `shouldReturn` PronNotAvailable
        it "should return NotFound when word not in dictionary" $ do
            DWDS.getMp3Url (Wort "nonexistent") `shouldReturn` NotFound

    describe "Duden.getMp3Url" $ do
        it "should retrieve URL of pron mp3" $ do
            Duden.getMp3Url (Wort "Bruder") `shouldReturn` PronFound (Mp3Url "https://www.duden.de/_media_/audio/ID4113233_375377226.mp3")
        it "should return PronNotAvailable when word has no pron" $ do
            Duden.getMp3Url (Wort "Ökumene") `shouldReturn` PronNotAvailable
        it "should return NotFound when word not in dictionary" $ do
            Duden.getMp3Url (Wort "nonexistent") `shouldReturn` NotFound
