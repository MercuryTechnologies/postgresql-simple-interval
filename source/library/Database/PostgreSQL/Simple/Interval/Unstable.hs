{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Simple.Interval.Unstable where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Ascii
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Function as Function
import qualified Data.Int as Int
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Database.Persist as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField as Postgres
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as Postgres

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
data Interval = MkInterval
  { months :: !Int.Int32,
    days :: !Int.Int32,
    microseconds :: !Int.Int64
  }
  deriving (Eq, Show)

-- | Uses 'parse'. Ensures that the OID is 'Postgres.intervalOid'.
instance Postgres.FromField Interval where
  fromField = Postgres.attoFieldParser (== Postgres.intervalOid) parse

-- | Uses 'render'. Always includes an @interval@ prefix, like
-- @interval '\@ 0 mon -1 day +2 us'@.
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

-- | Renders an interval to a 'Builder'. This always has the same format:
-- @"\@ X mon Y day Z us"@, where @X@, @Y@, and @Z@ are signed integers.
--
-- This is not the most compact format, but it is very easy to interpret and
-- does not require dealing with decimals (which could introduce precision
-- problems).
--
-- >>> render MkInterval { months = 0, days = -1, microseconds = 2 }
-- "@ 0 mon -1 day +2 us"
render :: Interval -> Builder.Builder
render x =
  let signed :: (Num a, Ord a) => (a -> Builder.Builder) -> a -> Builder.Builder
      signed f n = (if n > 0 then "+" else "") <> f n
   in "@ "
        <> signed Builder.int32Dec (months x)
        <> " mon "
        <> signed Builder.int32Dec (days x)
        <> " day "
        <> signed Builder.int64Dec (microseconds x)
        <> " us"

-- | Parses an interval. This is not a general purpose parser. It only supports
-- the formats that PostgreSQL generates. For example, it will fail to parse an
-- interval like @"1 week"@ because PostgreSQL never uses weeks when rendering
-- intervals.
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
  -- Both `-infinity` and `infinity` are new as of PostgreSQL 17.0.
  -- https://www.postgresql.org/message-id/E1r2rB1-005PHm-UL%40gemulon.postgresql.org
  A.choice
    [ MkInterval minBound minBound minBound <$ "-infinity",
      MkInterval maxBound maxBound maxBound <$ "infinity"
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
  Months m -> Months (-m)
  Days d -> Days (-d)
  Hours h -> Hours (-h)
  Minutes m -> Minutes (-m)
  Seconds u -> Seconds (-u)
  Microseconds u -> Microseconds (-u)

negateComponentsWhen :: (Functor f) => Bool -> f Component -> f Component
negateComponentsWhen p = if p then fmap negateComponent else id
