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
            [ (("pralinka", "e Praline (-, -n)", "", "y"), "Praline")
            , (("vydání (knihy ap.)", "e Auflage (-, -n)", "", "y"), "Auflage")
            , (("zálusk, choutky (na co)", "s Gelüste (auf 4.p)", "", "y"), "Gelüste")
            , (("připravit se na něco (2 předložky)", "(sich) vor/bereiten (auf etw 4.p / für etw)", "", "y"), "vorbereiten")
            , (("trpět (čím)<br />(min?)", "leiden (unter etw) - litt - hat gelitten ", "", "y"), "leiden")
            , (("zadržet, zastavit<br />(min?)<br />(er?)", "an/halten - hielt an - hat angehalten<br />er hält an", "", "y"), "anhalten")
            , (("zemřít (na něco)<br />(er?)<br />(min?)", "sterben (an etw - 3.p!) - starb - ist gestorben<br />er stirbt", "", "y"), "sterben")
            , (("napadnou (někoho) (o myšlence)<br />(min?)<br />(er?)", "ein/fallen (j-m) (3.p!) - fiel ein - ist eingefallen<br />fällt ein", "Dann <b>fiel</b> ihnen Frederick <b>ein</b>.<br>Es ist uns leider nichts <b>eingefallen</b>.", "y"), "einfallen")
            , (("prosit (někoho o něco)<br />(min?)", "bitten (j-n um etw) - bat - hat gebeten", "", "y"), "bitten")
            ]
        $ \((lang1, lang2, examples, yes), expectedWort) ->
            it ("should extract the word correctly: " <> expectedWort) $
                Types.extractWord (AnkiNote 1 lang1 lang2 examples yes "") `shouldBe` Wort expectedWort

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
