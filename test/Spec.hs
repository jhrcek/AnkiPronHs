{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (for_)
import Download qualified
import Search.DWDS qualified as DWDS
import Search.Duden qualified as Duden
import Search.VocabularyCom qualified as VocabularyCom
import Test.Hspec
import Types (AnkiNote (..), Mp3Url (..), SearchResult (..), Wort (..))
import Types qualified


main :: IO ()
main = hspec $ do
    describe "Types.extractWord"
        . for_
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
        $ \(flds, expectedWort) ->
            it ("should extract the word correctly: " <> expectedWort) $
                Types.extractWord (AnkiNote 1 flds "") `shouldBe` Wort expectedWort

    describe "DWDS.search" $ do
        it "should retrieve URL of pron mp3" $
            DWDS.search (Wort "Bär") `shouldReturn` PronFound (Mp3Url "https://www.dwds.de/audio/005/der_Baer.mp3")
        it "should return PronNotAvailable when word has no pron" $
            DWDS.search (Wort "Verlängerungskabel") `shouldReturn` PronNotAvailable
        it "should return NotFound when word not in dictionary" $
            DWDS.search (Wort "nonexistent") `shouldReturn` NotFound

    describe "Duden.search" $ do
        it "should retrieve URL of pron mp3" $
            Duden.search (Wort "Straße") `shouldReturn` PronFound (Mp3Url "https://cdn.duden.de/_media_/audio/ID4120277_502978504.mp3")
        it "should return PronNotAvailable when word has no pron" $
            Duden.search (Wort "Ökumene") `shouldReturn` PronNotAvailable
        it "should return NotFound when word not in dictionary" $
            Duden.search (Wort "nonexistent") `shouldReturn` NotFound

    describe "VocabularyCom.search" $ do
        it "should retrieve URL of pron mp3" $
            VocabularyCom.search (Wort "incontrovertible") `shouldReturn` PronFound (Mp3Url "https://audio.vocabulary.com/1.0/us/I/1D6AYF0J414VX.mp3")
        -- TODO
        it "should return PronNotAvailable when word has no pron" True

        it "should return NotFound when word not in dictionary" $
            VocabularyCom.search (Wort "blabla") `shouldReturn` NotFound

    describe "downloadMp3s" $
        it "should not trow an exception when download fails" $
            Download.downloadMp3s [(Wort "DUMMY", Mp3Url "https://cdn.duden.de/_media_/audio/ID4521392_440923517.mp3")]
