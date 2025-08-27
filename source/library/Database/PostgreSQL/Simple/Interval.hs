module Database.PostgreSQL.Simple.Interval
  ( Unstable.Interval (..),

    -- * Constructors
    Unstable.zero,
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

    -- * Arithmetic
    Unstable.negate,
    Unstable.negateSaturating,
    Unstable.add,
    Unstable.addSaturating,
  )
where

import qualified Database.PostgreSQL.Simple.Interval.Unstable as Unstable
