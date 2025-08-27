{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Proxy as Proxy
import qualified Database.PostgreSQL.LibPQ as Pq
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.PostgreSQL.Simple.Internal as Postgres
import qualified Database.PostgreSQL.Simple.Interval.Unstable as I
import qualified Database.PostgreSQL.Simple.ToField as Postgres
import qualified Test.Hspec as H

main :: IO ()
main = H.hspec spec

spec :: H.Spec
spec = H.describe "Database.PostgreSQL.Simple.Interval" $ do
  H.describe "zero" $ do
    H.it "works" $ do
      I.zero `H.shouldBe` I.MkInterval 0 0 0

  H.describe "add" $ do
    H.it "succeeds with no overflow" $ do
      let actual = I.add (I.MkInterval 1 2 3) (I.MkInterval 4 5 6)
      actual `H.shouldBe` Just (I.MkInterval 5 7 9)

    H.it "fails with positive month overflow" $ do
      let actual = I.add (I.fromMonths maxBound) (I.fromMonths 1)
      actual `H.shouldBe` Nothing

    H.it "fails with negative month overflow" $ do
      let actual = I.add (I.fromMonths minBound) (I.fromMonths (-1))
      actual `H.shouldBe` Nothing

    H.it "fails with positive day overflow" $ do
      let actual = I.add (I.fromDays maxBound) (I.fromDays 1)
      actual `H.shouldBe` Nothing

    H.it "fails with negative day overflow" $ do
      let actual = I.add (I.fromDays minBound) (I.fromDays (-1))
      actual `H.shouldBe` Nothing

    H.it "fails with positive microsecond overflow" $ do
      let actual = I.add (I.fromMicroseconds maxBound) (I.fromMicroseconds 1)
      actual `H.shouldBe` Nothing

    H.it "fails with negative microsecond overflow" $ do
      let actual = I.add (I.fromMicroseconds minBound) (I.fromMicroseconds (-1))
      actual `H.shouldBe` Nothing

  H.describe "fromMicroseconds" $ do
    H.it "works" $ do
      I.fromMicroseconds 1 `H.shouldBe` I.MkInterval 0 0 1

  H.describe "fromMilliseconds" $ do
    H.it "succeeds with no overflow" $ do
      I.fromMilliseconds 1 `H.shouldBe` Just (I.MkInterval 0 0 1e3)

    H.it "fails with overflow" $ do
      I.fromMilliseconds maxBound `H.shouldBe` Nothing

  H.describe "fromMillisecondsSaturating" $ do
    H.it "succeeds without saturating" $ do
      I.fromMillisecondsSaturating 1 `H.shouldBe` I.MkInterval 0 0 1e3

    H.it "succeeds with saturating" $ do
      I.fromMillisecondsSaturating 9223372036854776 `H.shouldBe` I.MkInterval 0 0 9223372036854775807

  H.describe "fromMillisecondsLiteral" $ do
    H.it "succeeds" $ do
      I.fromMillisecondsLiteral @1 Proxy.Proxy `H.shouldBe` I.MkInterval 0 0 1e3

  H.describe "fromSeconds" $ do
    H.it "succeeds with no overflow" $ do
      I.fromSeconds 1 `H.shouldBe` Just (I.MkInterval 0 0 1e6)

    H.it "fails with overflow" $ do
      I.fromSeconds maxBound `H.shouldBe` Nothing

  H.describe "fromSecondsSaturating" $ do
    H.it "succeeds without saturating" $ do
      I.fromSecondsSaturating 1 `H.shouldBe` I.MkInterval 0 0 1e6

    H.it "succeeds with saturating" $ do
      I.fromSecondsSaturating 9223372036855 `H.shouldBe` I.MkInterval 0 0 9223372036854775807

  H.describe "fromSecondsLiteral" $ do
    H.it "succeeds" $ do
      I.fromSecondsLiteral @1 Proxy.Proxy `H.shouldBe` I.MkInterval 0 0 1e6

  H.describe "fromMinutes" $ do
    H.it "succeeds with no overflow" $ do
      I.fromMinutes 1 `H.shouldBe` Just (I.MkInterval 0 0 60e6)

    H.it "fails with overflow" $ do
      I.fromMinutes maxBound `H.shouldBe` Nothing

  H.describe "fromMinutesSaturating" $ do
    H.it "succeeds without saturating" $ do
      I.fromMinutesSaturating 1 `H.shouldBe` I.MkInterval 0 0 60e6

    H.it "succeeds with saturating" $ do
      I.fromMinutesSaturating 153722867281 `H.shouldBe` I.MkInterval 0 0 9223372036854775807

  H.describe "fromMinutesLiteral" $ do
    H.it "succeeds" $ do
      I.fromMinutesLiteral @1 Proxy.Proxy `H.shouldBe` I.MkInterval 0 0 60e6

  H.describe "fromHours" $ do
    H.it "succeeds with no overflow" $ do
      I.fromHours 1 `H.shouldBe` Just (I.MkInterval 0 0 3600e6)

    H.it "fails with overflow" $ do
      I.fromHours maxBound `H.shouldBe` Nothing

  H.describe "fromHoursSaturating" $ do
    H.it "succeeds without saturating" $ do
      I.fromHoursSaturating 1 `H.shouldBe` I.MkInterval 0 0 3600e6

    H.it "succeeds with saturating" $ do
      I.fromHoursSaturating 2562047789 `H.shouldBe` I.MkInterval 0 0 9223372036854775807

  H.describe "fromHoursLiteral" $ do
    H.it "succeeds" $ do
      I.fromHoursLiteral @1 Proxy.Proxy `H.shouldBe` I.MkInterval 0 0 3600e6

  H.describe "fromDays" $ do
    H.it "works" $ do
      I.fromDays 1 `H.shouldBe` I.MkInterval 0 1 0

  H.describe "fromWeeks" $ do
    H.it "succeeds with no overflow" $ do
      I.fromWeeks 1 `H.shouldBe` Just (I.MkInterval 0 7 0)

    H.it "fails with overflow" $ do
      I.fromWeeks maxBound `H.shouldBe` Nothing

  H.describe "fromWeeksSaturating" $ do
    H.it "succeeds without saturating" $ do
      I.fromWeeksSaturating 1 `H.shouldBe` I.MkInterval 0 7 0

    H.it "succeeds with saturating" $ do
      I.fromWeeksSaturating 306783379 `H.shouldBe` I.MkInterval 0 2147483647 0

  H.describe "fromWeeksLiteral" $ do
    H.it "succeeds" $ do
      I.fromWeeksLiteral @1 Proxy.Proxy `H.shouldBe` I.MkInterval 0 7 0

  H.describe "fromMonths" $ do
    H.it "works" $ do
      I.fromMonths 1 `H.shouldBe` I.MkInterval 1 0 0

  H.describe "fromYears" $ do
    H.it "succeeds with no overflow" $ do
      I.fromYears 1 `H.shouldBe` Just (I.MkInterval 12 0 0)

    H.it "fails with overflow" $ do
      I.fromYears maxBound `H.shouldBe` Nothing

  H.describe "fromYearsSaturating" $ do
    H.it "succeeds without saturating" $ do
      I.fromYearsSaturating 1 `H.shouldBe` I.MkInterval 12 0 0

    H.it "succeeds with saturating" $ do
      I.fromYearsSaturating 178956971 `H.shouldBe` I.MkInterval 2147483647 0 0

  H.describe "fromYearsLiteral" $ do
    H.it "succeeds" $ do
      I.fromYearsLiteral @1 Proxy.Proxy `H.shouldBe` I.MkInterval 12 0 0

  H.describe "negate" $ do
    H.it "succeeds with no overflow" $ do
      I.negate (I.MkInterval 1 2 3) `H.shouldBe` Just (I.MkInterval (-1) (-2) (-3))

    H.it "fails with month overflow" $ do
      I.negate (I.MkInterval minBound 0 0) `H.shouldBe` Nothing

    H.it "fails with day overflow" $ do
      I.negate (I.MkInterval 0 minBound 0) `H.shouldBe` Nothing

    H.it "fails with microsecond overflow" $ do
      I.negate (I.MkInterval 0 0 minBound) `H.shouldBe` Nothing

  H.describe "negateSaturating" $ do
    H.it "succeeds without saturating" $ do
      I.negateSaturating (I.MkInterval 1 2 3) `H.shouldBe` I.MkInterval (-1) (-2) (-3)

    H.it "succeeds with saturating month" $ do
      I.negateSaturating (I.MkInterval minBound 0 0) `H.shouldBe` I.MkInterval maxBound 0 0

    H.it "succeeds with saturating day" $ do
      I.negateSaturating (I.MkInterval 0 minBound 0) `H.shouldBe` I.MkInterval 0 maxBound 0

    H.it "succeeds with saturating microsecond" $ do
      I.negateSaturating (I.MkInterval 0 0 minBound) `H.shouldBe` I.MkInterval 0 0 maxBound

  H.describe "render" $ do
    H.it "works with zero" $ do
      let actual = Builder.toLazyByteString $ I.render I.zero
      actual `H.shouldBe` "@ 0 mon 0 day 0 hour 0 min 0 sec 0 us"

    H.it "works with positive components" $ do
      let actual = Builder.toLazyByteString . I.render $ I.MkInterval 1 2 3
      actual `H.shouldBe` "@ +1 mon +2 day 0 hour 0 min 0 sec +3 us"

    H.it "works with negative components" $ do
      let actual = Builder.toLazyByteString . I.render $ I.MkInterval (-3) (-2) (-1)
      actual `H.shouldBe` "@ -3 mon -2 day 0 hour 0 min 0 sec -1 us"

    H.it "works with time components" $ do
      let actual = Builder.toLazyByteString . I.render $ I.MkInterval 0 0 3723000004
      actual `H.shouldBe` "@ 0 mon 0 day +1 hour +2 min +3 sec +4 us"

  H.describe "parse" $ do
    H.it "fails with invalid input" $ do
      let actual = Attoparsec.parseOnly I.parse "invalid"
      actual `H.shouldBe` Left "Failed reading: empty"

    H.it "succeeds with positive infinity" $ do
      let actual = Attoparsec.parseOnly I.parse "infinity"
      actual `H.shouldBe` Right (I.MkInterval maxBound maxBound maxBound)

    H.it "succeeds with negative infinity" $ do
      let actual = Attoparsec.parseOnly I.parse "-infinity"
      actual `H.shouldBe` Right (I.MkInterval minBound minBound minBound)

    Monad.forM_ intervalStyles $ \(_, field) ->
      Monad.forM_ examples $ \example -> do
        let input = field example
        H.it ("succeeds with " <> show input) $ do
          let actual = Attoparsec.parseOnly I.parse input
          actual `H.shouldBe` Right (exampleInterval example)

  H.describe "round trip" $ do
    Monad.forM_ examples $ \example -> do
      let interval = exampleInterval example
      let input = LazyByteString.toStrict . Builder.toLazyByteString $ I.render interval
      H.it ("succeeds with " <> show input) $ do
        let actual = Attoparsec.parseOnly I.parse input
        actual `H.shouldBe` Right interval

  H.describe "integration" $ do
    Monad.forM_ intervalStyles $ \(style, field) -> do
      H.describe ("with style " <> show style) $ do
        Monad.forM_ examples $ \example -> do
          H.it ("round trips " <> show (field example)) $ do
            Postgres.withConnect Postgres.defaultConnectInfo $ \connection -> do
              let interval = exampleInterval example
              result <- Exception.try . Postgres.withTransaction connection $ do
                Monad.void $ Postgres.execute connection "set local intervalstyle = ?" [style]
                Postgres.query connection "select ?" [interval]
              case result of
                Right actual -> actual `H.shouldBe` [Postgres.Only interval]
                Left somePostgresqlException -> do
                  version <- Postgres.withConnection connection Pq.serverVersion
                  if version < 150000
                    then H.pendingWith $ "interval parsing broken with PostgreSQL version " <> show version
                    else Exception.throwIO (somePostgresqlException :: Postgres.SomePostgreSqlException)

data IntervalStyle
  = Iso8601
  | Postgres
  | PostgresVerbose
  | SqlStandard
  deriving (Eq, Show)

instance Postgres.ToField IntervalStyle where
  toField style = Postgres.Plain $ case style of
    Iso8601 -> "iso_8601"
    Postgres -> "postgres"
    PostgresVerbose -> "postgres_verbose"
    SqlStandard -> "sql_standard"

data Example = MkExample
  { exampleInterval :: I.Interval,
    exampleIso8601 :: ByteString.ByteString,
    examplePostgres :: ByteString.ByteString,
    examplePostgresVerbose :: ByteString.ByteString,
    exampleSqlStandard :: ByteString.ByteString
  }
  deriving (Eq, Show)

intervalStyles :: [(IntervalStyle, Example -> ByteString.ByteString)]
intervalStyles =
  [ (Iso8601, exampleIso8601),
    (Postgres, examplePostgres),
    (PostgresVerbose, examplePostgresVerbose),
    (SqlStandard, exampleSqlStandard)
  ]

mkExample ::
  Int.Int32 ->
  Int.Int32 ->
  Int.Int64 ->
  ByteString.ByteString ->
  ByteString.ByteString ->
  ByteString.ByteString ->
  ByteString.ByteString ->
  Example
mkExample m d s iso8601 postgres postgresVerbose sqlStandard =
  MkExample
    { exampleInterval = I.MkInterval {I.months = m, I.days = d, I.microseconds = s},
      exampleIso8601 = iso8601,
      examplePostgres = postgres,
      examplePostgresVerbose = postgresVerbose,
      exampleSqlStandard = sqlStandard
    }

examples :: [Example]
examples =
  [ mkExample 0 0 0 "PT0S" "00:00:00" "@ 0" "0",
    mkExample 1 0 0 "P1M" "1 mon" "@ 1 mon" "0-1",
    mkExample (-1) 0 0 "P-1M" "-1 mons" "@ 1 mon ago" "-0-1",
    mkExample 3 0 0 "P3M" "3 mons" "@ 3 mons" "0-3",
    mkExample 6 0 0 "P6M" "6 mons" "@ 6 mons" "0-6",
    mkExample 12 0 0 "P1Y" "1 year" "@ 1 year" "1-0",
    mkExample (-12) 0 0 "P-1Y" "-1 years" "@ 1 year ago" "-1-0",
    mkExample 13 0 0 "P1Y1M" "1 year 1 mon" "@ 1 year 1 mon" "1-1",
    mkExample (-13) 0 0 "P-1Y-1M" "-1 years -1 mons" "@ 1 year 1 mon ago" "-1-1",
    mkExample 24 0 0 "P2Y" "2 years" "@ 2 years" "2-0",
    mkExample 0 1 0 "P1D" "1 day" "@ 1 day" "1 0:00:00",
    mkExample 0 (-1) 0 "P-1D" "-1 days" "@ 1 day ago" "-1 0:00:00",
    mkExample 0 2 0 "P2D" "2 days" "@ 2 days" "2 0:00:00",
    mkExample 0 7 0 "P7D" "7 days" "@ 7 days" "7 0:00:00",
    mkExample 0 0 1 "PT0.000001S" "00:00:00.000001" "@ 0.000001 secs" "0:00:00.000001",
    mkExample 0 0 (-1) "PT-0.000001S" "-00:00:00.000001" "@ 0.000001 secs ago" "-0:00:00.000001",
    mkExample 0 0 1e3 "PT0.001S" "00:00:00.001" "@ 0.001 secs" "0:00:00.001",
    mkExample 0 0 1e6 "PT1S" "00:00:01" "@ 1 sec" "0:00:01",
    mkExample 0 0 (-1e6) "PT-1S" "-00:00:01" "@ 1 sec ago" "-0:00:01",
    mkExample 0 0 2e6 "PT2S" "00:00:02" "@ 2 secs" "0:00:02",
    mkExample 0 0 60e6 "PT1M" "00:01:00" "@ 1 min" "0:01:00",
    mkExample 0 0 (-60e6) "PT-1M" "-00:01:00" "@ 1 min ago" "-0:01:00",
    mkExample 0 0 120e6 "PT2M" "00:02:00" "@ 2 mins" "0:02:00",
    mkExample 0 0 3600e6 "PT1H" "01:00:00" "@ 1 hour" "01:00:00",
    mkExample 0 0 (-3600e6) "PT-1H" "-01:00:00" "@ 1 hour ago" "-01:00:00",
    mkExample 0 0 7200e6 "PT2H" "02:00:00" "@ 2 hours" "02:00:00",
    mkExample 0 0 86400e6 "PT24H" "24:00:00" "@ 24 hours" "24:00:00",
    mkExample 1 1 1e6 "P1M1DT1S" "1 mon 1 day 00:00:01" "@ 1 mon 1 day 1 sec" "+0-1 +1 +0:00:01",
    mkExample (-1) (-1) (-1e6) "P-1M-1DT-1S" "-1 mons -1 days -00:00:01" "@ 1 mon 1 day 1 sec ago" "-0-1 -1 -0:00:01",
    mkExample (-1) 1 1e6 "P-1M1DT1S" "-1 mons +1 day 00:00:01" "@ 1 mon -1 days -1 sec ago" "-0-1 +1 +0:00:01",
    mkExample 1 (-1) 1e6 "P1M-1DT1S" "1 mon -1 days +00:00:01" "@ 1 mon -1 days 1 sec" "+0-1 -1 +0:00:01",
    mkExample 1 1 (-1e6) "P1M1DT-1S" "1 mon 1 day -00:00:01" "@ 1 mon 1 day -1 sec" "+0-1 +1 -0:00:01",
    mkExample 14 3 14706000007 "P1Y2M3DT4H5M6.000007S" "1 year 2 mons 3 days 04:05:06.000007" "@ 1 year 2 mons 3 days 4 hours 5 mins 6.000007 secs" "+1-2 +3 +4:05:06.000007",
    mkExample maxBound 0 0 "P178956970Y7M" "178956970 years 7 mons" "@ 178956970 years 7 mons" "178956970-7",
    mkExample minBound 0 0 "P-178956970Y-8M" "-178956970 years -8 mons" "@ 178956970 years 8 mons ago" "-178956970-8",
    mkExample 0 maxBound 0 "P2147483647D" "2147483647 days" "@ 2147483647 days" "2147483647 0:00:00",
    mkExample 0 minBound 0 "P-2147483648D" "-2147483648 days" "@ 2147483648 days ago" "-2147483648 0:00:00",
    mkExample 0 0 maxBound "PT2562047788H54.775807S" "2562047788:00:54.775807" "@ 2562047788 hours 54.775807 secs" "2562047788:00:54.775807",
    mkExample 0 0 minBound "PT-2562047788H-54.775808S" "-2562047788:00:54.775808" "@ 2562047788 hours 54.775808 secs ago" "-2562047788:00:54.775808"
  ]
