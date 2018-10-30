import Parser
import Test.Hspec
import Text.Trifecta

parse :: String -> Result SemVer
parse = parseString parseSemVer mempty

main :: IO ()
main = hspec $ do
  describe "Testing some SemVer examples" $ do
    it "2.0.0" $ do
      case parse "2.0.0" of
        Failure err -> fail (show err)
        Success res ->
          res `shouldBe` SemVer 2 0 0 [] []

    it "2.0.0-rc.2" $ do
      case parse "2.0.0-rc.2" of
        Failure err -> fail (show err)
        Success res ->
          res `shouldBe` SemVer 2 0 0 [NOSS "rc", NOSI 2] []

    it "1.0.0-beta" $ do
      case parse "1.0.0-beta" of
        Failure err -> fail (show err)
        Success res ->
          res `shouldBe` SemVer 1 0 0 [NOSS "beta"] []

    it "1.0.0-alpha+001" $ do
      case parse "1.0.0-alpha+001" of
        Failure err -> fail (show err)
        Success res ->
          res `shouldBe` SemVer 1 0 0 [NOSS "alpha"] [NOSI 1]

    it "1.0.0+20130313144700" $ do
      case parsse "1.0.0+20130313144700" of
        Failure err -> fail (show err)
        Success res ->
          res `shouldBe` SemVer 1 0 0 [] [NOSI 20130313144700]

    it "1.0.0-beta+exp.sha.5114f85" $ do
      case parse "1.0.0-beta+exp.sha.5114f85" of
        Failure err -> fail (show err)
        Success res ->
          res `shouldBe` SemVer 1 0 0 [NOSS "beta"]
              [NOSS "exp", NOSS "sha", NOSS "5114f85"]

    it "1.0.0-alpha.1" $ do
      case parse "1.0.0-alpha.1" of
        Failure err -> fail (show err)
        Success res ->
          res `shouldBe` SemVer 1 0 0 [NOSS "alpha", NOSI 1] []

    it "1.0.0-0.3.7" $ do
      case parse "1.0.0-0.3.7" of
        Failure err -> fail (show err)
        Success res ->
          res `shouldBe` SemVer 1 0 0 [NOSI 0, NOSI 3, NOSI 7] []

    it "1.0.0-x.7.z.92" $ do
      case parse "1.0.0-x.7.z.92" of
        Failure err -> fail (show err)
        Success res ->
          res `shouldBe` SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []

    it "6.6.6-alpha+exp.opa.0312ff03.1.2" $ do
      case parse "6.6.6-alpha+exp.opa.0312ff03.1.2" of
        Failure err -> fail (show err)
        Success err ->
          res `shouldBe` SemVer 6 6 6 [NOSS "alpha"]
              [NOSS "exp", NOSS "opa", NOSS "0312ff03", NOSI 1, NOSI 2]
