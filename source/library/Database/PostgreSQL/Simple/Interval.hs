module Database.PostgreSQL.Simple.Interval
  ( Unstable.Interval (..),

    -- * Construction
    Unstable.zero,
    Unstable.infinity,
    Unstable.negativeInfinity,
    Unstable.fromMicroseconds,
    Unstable.fromMilliseconds,
    Unstable.fromSeconds,
    Unstable.fromMinutes,
    Unstable.fromHours,
    Unstable.fromDays,
    Unstable.fromWeeks,
    Unstable.fromMonths,
    Unstable.fromYears,

    -- ** Saturating
    Unstable.fromMillisecondsSaturating,
    Unstable.fromSecondsSaturating,
    Unstable.fromMinutesSaturating,
    Unstable.fromHoursSaturating,
    Unstable.fromWeeksSaturating,
    Unstable.fromYearsSaturating,

    -- ** Literal
    Unstable.fromMillisecondsLiteral,
    Unstable.fromSecondsLiteral,
    Unstable.fromMinutesLiteral,
    Unstable.fromHoursLiteral,
    Unstable.fromWeeksLiteral,
    Unstable.fromYearsLiteral,

    -- * Conversion
    Unstable.intoTime,
    Unstable.fromTime,

    -- ** Saturating
    Unstable.fromTimeSaturating,

    -- * Arithmetic
    Unstable.add,
    Unstable.negate,
    Unstable.scale,

    -- ** Saturating
    Unstable.addSaturating,
    Unstable.negateSaturating,
    Unstable.scaleSaturating,
  )
where

import qualified Database.PostgreSQL.Simple.Interval.Unstable as Unstable
