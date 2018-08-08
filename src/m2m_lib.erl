-module(m2m_lib).
-export([timestamp_to_string/1]).

timestamp_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
      io_lib:format("~4..0w~2..0w~2..0w_~2..0w~2..0w~2..0w",
            [Year, Month, Day, Hour, Minute, Second])).