{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Database.PostgreSQL.Simple.Interval.Unstable where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Ascii
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Fixed as Fixed
import qualified Data.Function as Function
import qualified Data.Int as Int
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.Persist as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField as Postgres
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Postgres
import qualified GHC.TypeLits as TypeLits
import qualified Language.Haskell.TH.Syntax as TH

-- | This type represents a PostgreSQL interval. Intervals can have month, day,
-- and microsecond components. Each component is bounded, so they are not
-- arbitrary precision. For more information about intervals, consult the
-- PostgreSQL documentation:
-- <https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-INTERVAL-INPUT>.
--
-- Note that the @time@ library provides several duration types that are not
-- appropriate to use as PostgreSQL intervals:
--
-- - 'Data.Time.NominalDiffTime': Does not handle days or months. Allows up to
--   picosecond precision. Is not bounded.
-- - 'Data.Time.CalendarDiffTime': Does not handle days. Embeds a
--   @NominalDiffTime@. Is not bounded.
-- - 'Data.Time.CalendarDiffDays': Does not handle seconds. Is not bounded.
--
-- WARNING: The PostgreSQL interval parser is broken in versions prior to 15.
-- It is not possible to round trip all intervals through PostgreSQL on those
-- versions. You should upgrade to at least PostgreSQL version 15. For more
-- information, see this patch:
-- <https://git.postgresql.org/gitweb/?p=postgresql.git;a=commitdiff;h=e39f99046>
data Interval = MkInterval
  { months :: !Int.Int32,
    days :: !Int.Int32,
    microseconds :: !Int.Int64
  }
  deriving (Eq, TH.Lift, Show)

-- | Uses 'parse'. Ensures that the OID is 'Postgres.intervalOid'.
instance Postgres.FromField Interval where
  fromField = Postgres.attoFieldParser (== Postgres.intervalOid) parse

-- | Uses 'render'. Always includes an @interval@ prefix, like
-- @interval '...'@.
instance Postgres.ToField Interval where
  toField = Postgres.Plain . ("interval '" <>) . (<> "'") . render

-- | Behaves the same as the 'Postgres.FromField' and 'Postgres.ToField'
-- instances.
instance Persist.PersistField Interval where
  fromPersistValue persistValue = case persistValue of
    Persist.PersistLiteralEscaped byteString
      | Right interval <- A.parseOnly parse byteString ->
          Right interval
    Persist.PersistLiteral byteString
      | Just withoutPrefix <- Ascii.stripPrefix "interval '" byteString,
        Just withoutSuffix <- Ascii.stripSuffix "'" withoutPrefix,
        Right interval <- A.parseOnly parse withoutSuffix ->
          Right interval
    _ -> Left $ "Invalid interval: " <> Text.pack (show persistValue)
  toPersistValue =
    Persist.PersistLiteral
      . LazyByteString.toStrict
      . Builder.toLazyByteString
      . ("interval '" <>)
      . (<> "'")
      . render

-- | @'Persist.SqlOther' "interval"@
instance Persist.PersistFieldSql Interval where
  sqlType = const $ Persist.SqlOther "interval"

-- | The empty interval, representing no time at all.
--
-- >>> zero
-- MkInterval {months = 0, days = 0, microseconds = 0}
zero :: Interval
zero = MkInterval 0 0 0

-- | The biggest possible interval.
--
-- >>> infinity
-- MkInterval {months = 2147483647, days = 2147483647, microseconds = 9223372036854775807}
infinity :: Interval
infinity = MkInterval maxBound maxBound maxBound

-- | The smallest possible interval.
--
-- >>> negativeInfinity
-- MkInterval {months = -2147483648, days = -2147483648, microseconds = -9223372036854775808}
negativeInfinity :: Interval
negativeInfinity = MkInterval minBound minBound minBound

-- | Creates an interval from a number of microseconds.
--
-- >>> fromMicroseconds 1
-- MkInterval {months = 0, days = 0, microseconds = 1}
fromMicroseconds :: Int.Int64 -> Interval
fromMicroseconds x = zero {microseconds = x}

-- | Creates an interval from a number of milliseconds. Returns 'Nothing' if
-- the interval would overflow.
--
-- >>> fromMilliseconds 1
-- Just (MkInterval {months = 0, days = 0, microseconds = 1000})
-- >>> fromMilliseconds 9223372036854776
-- Nothing
fromMilliseconds :: Int.Int64 -> Maybe Interval
fromMilliseconds =
  fmap fromMicroseconds
    . Bits.toIntegralSized
    . (* 1e3)
    . toInteger

-- | Like 'fromMilliseconds' but uses saturating arithmetic rather than
-- returning 'Maybe'.
--
-- >>> fromMillisecondsSaturating 1
-- MkInterval {months = 0, days = 0, microseconds = 1000}
-- >>> fromMillisecondsSaturating 9223372036854776
-- MkInterval {months = 0, days = 0, microseconds = 9223372036854775807}
fromMillisecondsSaturating :: Int.Int64 -> Interval
fromMillisecondsSaturating =
  fromMicroseconds
    . toIntegralSaturating
    . (* 1e3)
    . toInteger

-- | Like 'fromMilliseconds' but takes a type-level natural number as input.
-- This is useful for writing literals without risk of overflow.
--
-- >>> fromMillisecondsLiteral (Proxy :: Proxy 1)
-- MkInterval {months = 0, days = 0, microseconds = 1000}
fromMillisecondsLiteral ::
  (TypeLits.KnownNat n, (TypeLits.<=) n 9223372036854775) =>
  proxy n ->
  Interval
fromMillisecondsLiteral =
  fromMillisecondsSaturating
    . fromInteger
    . TypeLits.natVal

-- | Creates an interval from a number of seconds. Returns 'Nothing' if the
-- interval would overflow.
--
-- >>> fromSeconds 1
-- Just (MkInterval {months = 0, days = 0, microseconds = 1000000})
-- >>> fromSeconds 9223372036855
-- Nothing
fromSeconds :: Int.Int64 -> Maybe Interval
fromSeconds =
  fmap fromMicroseconds
    . Bits.toIntegralSized
    . (* 1e6)
    . toInteger

-- | Like 'fromSeconds' but uses saturating arithmetic rather than returning
-- 'Maybe'.
--
-- >>> fromSecondsSaturating 1
-- MkInterval {months = 0, days = 0, microseconds = 1000000}
-- >>> fromSecondsSaturating 9223372036855
-- MkInterval {months = 0, days = 0, microseconds = 9223372036854775807}
fromSecondsSaturating :: Int.Int64 -> Interval
fromSecondsSaturating =
  fromMicroseconds
    . toIntegralSaturating
    . (* 1e6)
    . toInteger

-- | Like 'fromSeconds' but takes a type-level natural number as input.
-- This is useful for writing literals without risk of overflow.
--
-- >>> fromSecondsLiteral (Proxy :: Proxy 1)
-- MkInterval {months = 0, days = 0, microseconds = 1000000}
fromSecondsLiteral ::
  (TypeLits.KnownNat n, (TypeLits.<=) n 9223372036854) =>
  proxy n ->
  Interval
fromSecondsLiteral =
  fromSecondsSaturating
    . fromInteger
    . TypeLits.natVal

-- | Creates an interval from a number of minutes. Returns 'Nothing' if the
-- interval would overflow.
--
-- >>> fromMinutes 1
-- Just (MkInterval {months = 0, days = 0, microseconds = 60000000})
-- >>> fromMinutes 153722867281
-- Nothing
fromMinutes :: Int.Int64 -> Maybe Interval
fromMinutes =
  fmap fromMicroseconds
    . Bits.toIntegralSized
    . (* 60e6)
    . toInteger

-- | Like 'fromMinutes' but uses saturating arithmetic rather than returning
-- 'Maybe'.
--
-- >>> fromMinutesSaturating 1
-- MkInterval {months = 0, days = 0, microseconds = 60000000}
-- >>> fromMinutesSaturating 153722867281
-- MkInterval {months = 0, days = 0, microseconds = 9223372036854775807}
fromMinutesSaturating :: Int.Int64 -> Interval
fromMinutesSaturating =
  fromMicroseconds
    . toIntegralSaturating
    . (* 60e6)
    . toInteger

-- | Like 'fromMinutes' but takes a type-level natural number as input.
-- This is useful for writing literals without risk of overflow.
--
-- >>> fromMinutesLiteral (Proxy :: Proxy 1)
-- MkInterval {months = 0, days = 0, microseconds = 60000000}
fromMinutesLiteral ::
  (TypeLits.KnownNat n, (TypeLits.<=) n 153722867280) =>
  proxy n ->
  Interval
fromMinutesLiteral =
  fromMinutesSaturating
    . fromInteger
    . TypeLits.natVal

-- | Creates an interval from a number of hours. Returns 'Nothing' if the
-- interval would overflow.
--
-- >>> fromHours 1
-- Just (MkInterval {months = 0, days = 0, microseconds = 3600000000})
-- >>> fromHours 2562047789
-- Nothing
fromHours :: Int.Int64 -> Maybe Interval
fromHours =
  fmap fromMicroseconds
    . Bits.toIntegralSized
    . (* 3600e6)
    . toInteger

-- | Like 'fromHours' but uses saturating arithmetic rather than returning
-- 'Maybe'.
--
-- >>> fromHoursSaturating 1
-- MkInterval {months = 0, days = 0, microseconds = 3600000000}
-- >>> fromHoursSaturating 2562047789
-- MkInterval {months = 0, days = 0, microseconds = 9223372036854775807}
fromHoursSaturating :: Int.Int64 -> Interval
fromHoursSaturating =
  fromMicroseconds
    . toIntegralSaturating
    . (* 3600e6)
    . toInteger

-- | Like 'fromHours' but takes a type-level natural number as input.
-- This is useful for writing literals without risk of overflow.
--
-- >>> fromHoursLiteral (Proxy :: Proxy 1)
-- MkInterval {months = 0, days = 0, microseconds = 3600000000}
fromHoursLiteral ::
  (TypeLits.KnownNat n, (TypeLits.<=) n 2562047788) =>
  proxy n ->
  Interval
fromHoursLiteral =
  fromHoursSaturating
    . fromInteger
    . TypeLits.natVal

-- | Creates an interval from a number of days.
--
-- >>> fromDays 1
-- MkInterval {months = 0, days = 1, microseconds = 0}
fromDays :: Int.Int32 -> Interval
fromDays x = zero {days = x}

-- | Creates an interval from a number of weeks. Returns 'Nothing' if the
-- interval would overflow.
--
-- >>> fromWeeks 1
-- Just (MkInterval {months = 0, days = 7, microseconds = 0})
-- >>> fromWeeks 306783379
-- Nothing
fromWeeks :: Int.Int32 -> Maybe Interval
fromWeeks =
  fmap fromDays
    . Bits.toIntegralSized
    . (* 7)
    . toInteger

-- | Like 'fromWeeks' but uses saturating arithmetic rather than returning
-- 'Maybe'.
--
-- >>> fromWeeksSaturating 1
-- MkInterval {months = 0, days = 7, microseconds = 0}
-- >>> fromWeeksSaturating 306783379
-- MkInterval {months = 0, days = 2147483647, microseconds = 0}
fromWeeksSaturating :: Int.Int32 -> Interval
fromWeeksSaturating =
  fromDays
    . toIntegralSaturating
    . (* 7)
    . toInteger

-- | Like 'fromWeeks' but takes a type-level natural number as input.
-- This is useful for writing literals without risk of overflow.
--
-- >>> fromWeeksLiteral (Proxy :: Proxy 1)
-- MkInterval {months = 0, days = 7, microseconds = 0}
fromWeeksLiteral ::
  (TypeLits.KnownNat n, (TypeLits.<=) n 306783378) =>
  proxy n ->
  Interval
fromWeeksLiteral =
  fromWeeksSaturating
    . fromInteger
    . TypeLits.natVal

-- | Creates an interval from a number of months.
--
-- >>> fromMonths 1
-- MkInterval {months = 1, days = 0, microseconds = 0}
fromMonths :: Int.Int32 -> Interval
fromMonths x = zero {months = x}

-- | Creates an interval from a number of years. Returns 'Nothing' if the
-- interval would overflow.
--
-- >>> fromYears 1
-- Just (MkInterval {months = 12, days = 0, microseconds = 0})
-- >>> fromYears 178956971
-- Nothing
fromYears :: Int.Int32 -> Maybe Interval
fromYears =
  fmap fromMonths
    . Bits.toIntegralSized
    . (* 12)
    . toInteger

-- | Like 'fromYears' but uses saturating arithmetic rather than returning
-- 'Maybe'.
--
-- >>> fromYearsSaturating 1
-- MkInterval {months = 12, days = 0, microseconds = 0}
-- >>> fromYearsSaturating 178956971
-- MkInterval {months = 2147483647, days = 0, microseconds = 0}
fromYearsSaturating :: Int.Int32 -> Interval
fromYearsSaturating =
  fromMonths
    . toIntegralSaturating
    . (* 12)
    . toInteger

-- | Like 'fromYears' but takes a type-level natural number as input.
-- This is useful for writing literals without risk of overflow.
--
-- >>> fromYearsLiteral (Proxy :: Proxy 1)
-- MkInterval {months = 12, days = 0, microseconds = 0}
fromYearsLiteral ::
  (TypeLits.KnownNat n, (TypeLits.<=) n 178956970) =>
  proxy n ->
  Interval
fromYearsLiteral =
  fromYearsSaturating
    . fromInteger
    . TypeLits.natVal

-- | Negates an interval. Returns 'Nothing' if the result would overflow.
--
-- >>> negate (MkInterval 1 2 3)
-- Just (MkInterval {months = -1, days = -2, microseconds = -3})
-- >>> negate (MkInterval (-2147483648) 0 0)
-- Nothing
negate :: Interval -> Maybe Interval
negate = scale (-1)

-- | Like 'Database.PostgreSQL.Simple.Interval.Unstable.negate' but uses
-- saturating arithmetic rather than returning 'Maybe'.
--
-- >>> negateSaturating (MkInterval 1 2 3)
-- MkInterval {months = -1, days = -2, microseconds = -3}
-- >>> negateSaturating (MkInterval (-2147483648) 0 0)
-- MkInterval {months = 2147483647, days = 0, microseconds = 0}
negateSaturating :: Interval -> Interval
negateSaturating = scaleSaturating (-1)

-- | Adds two intervals. Returns 'Nothing' if the result would overflow.
--
-- >>> add (fromMonths 1) (fromDays 2)
-- Just (MkInterval {months = 1, days = 2, microseconds = 0})
-- >>> add (fromDays 2147483647) (fromDays 1)
-- Nothing
add :: Interval -> Interval -> Maybe Interval
add x y =
  let safeAdd :: (Bits.Bits a, Integral a) => a -> a -> Maybe a
      safeAdd n = Bits.toIntegralSized . Function.on (+) toInteger n
   in MkInterval
        <$> Function.on safeAdd months x y
        <*> Function.on safeAdd days x y
        <*> Function.on safeAdd microseconds x y

-- | Like 'add' but uses saturating arithmetic rather than returning 'Maybe'.
--
-- >>> addSaturating (fromMonths 1) (fromDays 2)
-- MkInterval {months = 1, days = 2, microseconds = 0}
-- >>> addSaturating (fromDays 2147483647) (fromDays 1)
-- MkInterval {months = 0, days = 2147483647, microseconds = 0}
addSaturating :: Interval -> Interval -> Interval
addSaturating x y =
  let safeAdd :: (Bounded a, Integral a) => a -> a -> a
      safeAdd n = toIntegralSaturating . Function.on (+) toInteger n
   in MkInterval
        (Function.on safeAdd months x y)
        (Function.on safeAdd days x y)
        (Function.on safeAdd microseconds x y)

-- | Converts an interval into types from the @time@ library. See 'fromTime'
-- for the opposite conversion.
--
-- >>> intoTime (MkInterval 1 2 3)
-- (P1M2D,0.000003s)
intoTime :: Interval -> (Time.CalendarDiffDays, Time.NominalDiffTime)
intoTime x =
  ( Time.CalendarDiffDays
      { Time.cdMonths = toInteger $ months x,
        Time.cdDays = toInteger $ days x
      },
    Time.secondsToNominalDiffTime
      . realToFrac
      . intoMicro
      . toInteger
      $ microseconds x
  )

-- | Converts types from the @time@ library into an interval. See 'intoTime'
-- for the opposite conversion.
--
-- >>> fromTime ('Time.CalendarDiffDays' 1 2) 3
-- Just (MkInterval {months = 1, days = 2, microseconds = 3000000})
--
-- Returns 'Nothing' if the result would overflow. See 'fromTimeSaturating' for
-- a version that uses saturating arithmetic instead.
--
-- >>> fromTime mempty 9223372036854.775808
-- Nothing
--
-- Note that this truncates extra precision.
--
-- >>> fromTime mempty 0.0000009
-- Just (MkInterval {months = 0, days = 0, microseconds = 0})
fromTime :: Time.CalendarDiffDays -> Time.NominalDiffTime -> Maybe Interval
fromTime x y =
  MkInterval
    <$> Bits.toIntegralSized (Time.cdMonths x)
    <*> Bits.toIntegralSized (Time.cdDays x)
    <*> Bits.toIntegralSized (fromMicro . realToFrac $ Time.nominalDiffTimeToSeconds y)

-- | Like 'fromTime' but uses saturating arithmetic rather than returning
-- 'Maybe'.
--
-- >>> fromTimeSaturating ('Time.CalendarDiffDays' 1 2) 3
-- MkInterval {months = 1, days = 2, microseconds = 3000000}
-- >>> fromTimeSaturating mempty 9223372036854.775808
-- MkInterval {months = 1, days = 2, microseconds = 9223372036854775807}
fromTimeSaturating :: Time.CalendarDiffDays -> Time.NominalDiffTime -> Interval
fromTimeSaturating x y =
  MkInterval
    (toIntegralSaturating $ Time.cdMonths x)
    (toIntegralSaturating $ Time.cdDays x)
    (toIntegralSaturating . fromMicro . realToFrac $ Time.nominalDiffTimeToSeconds y)

intoMicro :: Integer -> Fixed.Micro
intoMicro = Fixed.MkFixed

fromMicro :: Fixed.Micro -> Integer
fromMicro (Fixed.MkFixed x) = x

-- | Scales an interval by the given ratio.
--
-- >>> scale 0.5 (MkInterval 2 4 8)
-- Just (MkInterval {months = 1, days = 2, microseconds = 4})
-- >>> scale 2 (MkInterval 2 4 8)
-- Just (MkInterval {months = 4, days = 8, microseconds = 16})
--
-- Each component is rounded.
--
-- >>> scale 0.4 (MkInterval 0 0 1) -- rounds down
-- Just (MkInterval {months = 0, days = 0, microseconds = 0})
-- >>> scale 0.5 (MkInterval 0 0 1) -- rounds half to even
-- Just (MkInterval {months = 0, days = 0, microseconds = 0})
-- >>> scale 0.6 (MkInterval 0 0 1) -- rounds up
-- Just (MkInterval {months = 0, days = 0, microseconds = 1})
--
-- Fractional days are converted into microseconds, assuming 24 hours per day.
--
-- >>> scale 0.5 (MkInterval 0 1 0)
-- Just (MkInterval {months = 0, days = 0, microseconds = 43200000000})
--
-- Fractional months are converted into days, assuming 30 days per month.
--
-- >>> scale 0.5 (MkInterval 1 0 0)
-- Just (MkInterval {months = 0, days = 15, microseconds = 0})
--
-- If this conversion produces fractional days, those are converted into
-- microseconds.
--
-- >>> scale 0.05 (MkInterval 1 0 0)
-- Just (MkInterval {months = 0, days = 1, microseconds = 43200000000})
--
-- Returns 'Nothing' if any component would overflow. See 'scaleSaturating' for
-- a version that uses saturating arithmetic instead.
--
-- >>> scale 2 (MkInterval 0 0 4611686018427387904)
-- Nothing
--
-- Note that due to rounding and conversion, scaling down and then up will not
-- necessarily return the original interval.
--
-- >>> fmap (scale 2) (scale 0.5 (MkInterval 0 0 1))
-- Just (Just (MkInterval {months = 0, days = 0, microseconds = 0}))
-- >>> fmap (scale 2) (scale 0.5 (MkInterval 1 0 0))
-- Just (Just (MkInterval {months = 0, days = 30, microseconds = 0}))
scale :: Rational -> Interval -> Maybe Interval
scale r i = do
  let (m :: Integer, mf) = properFraction . (*) r . toRational $ months i
  let (d :: Integer, uf) = properFraction . (+) (mf * 30) . (*) r . toRational $ days i
  let u :: Integer = round . (+) (uf * 86400000000) . (*) r . toRational $ microseconds i
  MkInterval
    <$> Bits.toIntegralSized m
    <*> Bits.toIntegralSized d
    <*> Bits.toIntegralSized u

-- | Like 'scale' but uses saturating arithmetic rather than returning
-- 'Nothing' on overflow.
--
-- >>> scaleSaturating 2 (MkInterval 0 0 4611686018427387904)
-- MkInterval {months = 0, days = 0, microseconds = 9223372036854775807}
scaleSaturating :: Rational -> Interval -> Interval
scaleSaturating r i =
  let (m :: Integer, mf) = properFraction . (*) r . toRational $ months i
      (d :: Integer, uf) = properFraction . (+) (mf * 30) . (*) r . toRational $ days i
      u :: Integer = round . (+) (uf * 86400000000) . (*) r . toRational $ microseconds i
   in MkInterval
        (toIntegralSaturating m)
        (toIntegralSaturating d)
        (toIntegralSaturating u)

-- | Renders an interval to a 'Builder'. This always has the same format:
-- @"\@ A mon B day C hour D min E sec F us"@, where @A@, @B@, @C@, @D@, @E@,
-- and @F@ are signed integers.
--
-- This is not the most compact format, but it is very easy to interpret and
-- does not require dealing with decimals (which could introduce precision
-- problems).
--
-- >>> render MkInterval { months = 0, days = -1, microseconds = 2 }
-- "@ 0 mon -1 day 0 hour 0 min 0 sec +2 us"
render :: Interval -> Builder.Builder
render x =
  let signed :: (Num a, Ord a) => (a -> Builder.Builder) -> a -> Builder.Builder
      signed f n = (if n > 0 then "+" else "") <> f n
      (t1, u) = quotRem (microseconds x) 1000000
      (t2, s) = quotRem t1 60
      (h, m) = quotRem t2 60
   in "@ "
        <> signed Builder.int32Dec (months x)
        <> " mon "
        <> signed Builder.int32Dec (days x)
        <> " day "
        <> signed Builder.int64Dec h
        <> " hour "
        <> signed Builder.int64Dec m
        <> " min "
        <> signed Builder.int64Dec s
        <> " sec "
        <> signed Builder.int64Dec u
        <> " us"

-- | Parses an interval. This is not a general purpose parser. It only supports
-- the formats that PostgreSQL generates.
parse :: A.Parser Interval
parse =
  -- Start with parsers that have non-empty prefixes, in order to avoid
  -- ambiguity. Neither of the `postgres` nor `sql_standard` interval styles
  -- have a prefix (or suffix), so whichever one is attempted first needs to
  -- make sure it has consumed all of the input.
  A.choice $
    parseInfinities
      : fmap
        (fromComponents =<<)
        [ parseIso8601,
          parsePostgresVerbose,
          parsePostgres <* A.endOfInput,
          parseSqlStandard <* A.endOfInput
        ]

parseInfinities :: A.Parser Interval
parseInfinities =
  -- `infinity` is new as of PostgreSQL 17.0.
  -- https://www.postgresql.org/message-id/E1r2rB1-005PHm-UL%40gemulon.postgresql.org
  A.choice
    [ negativeInfinity <$ "-infinity",
      infinity <$ "+infinity",
      infinity <$ "infinity"
    ]

parseIso8601 :: A.Parser [Component]
parseIso8601 = do
  Monad.void "P"
  dates <-
    A.many' $
      A.choice
        [ Years <$> A.signed A.decimal <* "Y",
          Months <$> A.signed A.decimal <* "M",
          Days <$> A.signed A.decimal <* "D"
        ]
  times <- A.option [] $ do
    Monad.void "T"
    A.many' $
      A.choice
        [ Hours <$> A.signed A.decimal <* "H",
          Minutes <$> A.signed A.decimal <* "M",
          Seconds <$> A.signed A.scientific <* "S"
        ]
  pure $ dates <> times

parsePostgresVerbose :: A.Parser [Component]
parsePostgresVerbose = do
  Monad.void "@ "
  components <-
    flip A.sepBy " " $
      A.choice
        [ Years <$> A.signed A.decimal <* maybePlural " year",
          Weeks <$> A.signed A.decimal <* maybePlural " week",
          Months <$> A.signed A.decimal <* maybePlural " mon",
          Days <$> A.signed A.decimal <* maybePlural " day",
          Hours <$> A.signed A.decimal <* maybePlural " hour",
          Minutes <$> A.signed A.decimal <* maybePlural " min",
          Microseconds <$> A.signed A.decimal <* " us",
          Seconds <$> A.signed A.scientific <* A.option "" (maybePlural " sec")
        ]
  ago <- A.option "" " ago"
  pure $ negateComponentsWhen (not $ ByteString.null ago) components

parsePostgres :: A.Parser [Component]
parsePostgres = do
  dates <-
    flip A.sepBy " " $
      A.choice
        [ Years <$> A.signed A.decimal <* maybePlural " year",
          Weeks <$> A.signed A.decimal <* maybePlural " week",
          Months <$> A.signed A.decimal <* maybePlural " mon",
          Days <$> A.signed A.decimal <* maybePlural " day"
        ]
  time <- A.option [] $ A.skipSpace *> parseTime
  pure $ dates <> time

parseSqlStandard :: A.Parser [Component]
parseSqlStandard = do
  let parseYearsAndMonths = do
        sign <- parseSign
        years <- Years <$> A.decimal <* "-"
        months_ <- Months <$> A.decimal
        pure $ negateComponentsWhen (sign == "-") [years, months_]
  let parseDays = (: []) . Days <$> A.signed A.decimal
  let parsers = [parseYearsAndMonths, parseTime, parseDays]
  mconcat <$> A.sepBy1 (A.choice parsers) " "

parseTime :: A.Parser [Component]
parseTime = do
  sign <- parseSign
  hours <- Hours <$> A.decimal <* ":"
  minutes <- Minutes <$> A.decimal <* ":"
  seconds <- Seconds <$> A.scientific
  pure $ negateComponentsWhen (sign == "-") [hours, minutes, seconds]

parseSign :: A.Parser ByteString.ByteString
parseSign = A.choice ["-", "+", ""]

maybePlural :: ByteString.ByteString -> A.Parser ByteString.ByteString
maybePlural word = (<>) <$> A.string word <*> A.option "" "s"

-- | One component of an interval. This is used to retain arbitrary precision
-- for as long as possible before converting. It also shows which components
-- are accepted, like years and months.
data Component
  = Years !Integer
  | Weeks !Integer
  | Months !Integer
  | Days !Integer
  | Hours !Integer
  | Minutes !Integer
  | Seconds !Scientific.Scientific
  | Microseconds !Integer
  deriving (Eq, Show)

-- | Converts a 'Component' to an 'Interval'. Returns 'Nothing' if the
-- component would overflow.
fromComponent :: Component -> Maybe Interval
fromComponent c = case c of
  Years y -> fromYears =<< Bits.toIntegralSized y
  Weeks w -> fromWeeks =<< Bits.toIntegralSized w
  Months m -> fromMonths <$> Bits.toIntegralSized m
  Days d -> fromDays <$> Bits.toIntegralSized d
  Hours h -> fromHours =<< Bits.toIntegralSized h
  Minutes m -> fromMinutes =<< Bits.toIntegralSized m
  Seconds u -> fromMicroseconds <$> Scientific.toBoundedInteger (u * 1e6)
  Microseconds u -> fromMicroseconds <$> Bits.toIntegralSized u

-- | Converts a list of 'Component's to an 'Interval'. Returns 'Nothing' if any
-- of the components would overflow, or if adding any of them together would
-- overflow.
fromComponents ::
  (Applicative.Alternative f, Traversable t) =>
  t Component ->
  f Interval
fromComponents =
  maybe Applicative.empty pure
    . (Monad.foldM add zero Monad.<=< traverse fromComponent)

negateComponent :: Component -> Component
negateComponent c = case c of
  Years y -> Years (-y)
  Weeks w -> Weeks (-w)
  Months m -> Months (-m)
  Days d -> Days (-d)
  Hours h -> Hours (-h)
  Minutes m -> Minutes (-m)
  Seconds u -> Seconds (-u)
  Microseconds u -> Microseconds (-u)

negateComponentsWhen :: (Functor f) => Bool -> f Component -> f Component
negateComponentsWhen p = if p then fmap negateComponent else id

toIntegralSaturating :: forall a b. (Integral a, Integral b, Bounded b) => a -> b
toIntegralSaturating x = case toInteger x of
  y
    | let lo = minBound :: b, y < toInteger lo -> lo
    | let hi = maxBound :: b, y > toInteger hi -> hi
    | otherwise -> fromInteger y
