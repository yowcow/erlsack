-module(erlsack_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/erlsack.hrl").

fill_test_() ->
    Cases = [
             {
              "capacity = 0",
              0,
              [
               #erlsack_item{value = 1, weight = 1, item = hoge}
              ],
              {0, []}
             },
             {
              "No items",
              10,
              [],
              {0, []}
             },
             {
              "1 item when capacity >= weight",
              1,
              [
               #erlsack_item{value = 1, weight = 1, item = hoge}
              ],
              {1, [
                   #erlsack_item{value = 1, weight = 1, item = hoge}
                  ]}
             },
             {
              "1 item when capacity < weight",
              1,
              [
               #erlsack_item{value = 1, weight = 2, item = hoge}
              ],
              {0, []}
             },
             {
              "item count within capacity",
              10,
              [
               #erlsack_item{value = 1, weight = 1, item = hoge},
               #erlsack_item{value = 4, weight = 4, item = fuga},
               #erlsack_item{value = 5, weight = 5, item = foo}
              ],
              {10, [
                    #erlsack_item{value = 5, weight = 5, item = foo},
                    #erlsack_item{value = 4, weight = 4, item = fuga},
                    #erlsack_item{value = 1, weight = 1, item = hoge}
                   ]}
             },
             {
              "item count over capacity (1)",
              6,
              [
               #erlsack_item{value = 5, weight = 4, item = hoge},
               #erlsack_item{value = 4, weight = 3, item = fuga},
               #erlsack_item{value = 3, weight = 2, item = foo},
               #erlsack_item{value = 2, weight = 1, item = bar}
              ],
              {9, [
                   #erlsack_item{value = 2, weight = 1, item = bar},
                   #erlsack_item{value = 3, weight = 2, item = foo},
                   #erlsack_item{value = 4, weight = 3, item = fuga}
                  ]}
             },
             {
              "item count over capacity 6 (2)",
              6,
              [
               #erlsack_item{value = 7, weight = 4, item = hoge},
               #erlsack_item{value = 4, weight = 3, item = fuga},
               #erlsack_item{value = 3, weight = 2, item = foo},
               #erlsack_item{value = 2, weight = 1, item = bar}
              ],
              {10, [
                    #erlsack_item{value = 3, weight = 2, item = foo},
                    #erlsack_item{value = 7, weight = 4, item = hoge}
                   ]}
             },
             {
              "no item within capacity",
              1,
              [
               #erlsack_item{value = 7, weight = 4, item = hoge},
               #erlsack_item{value = 4, weight = 3, item = fuga},
               #erlsack_item{value = 3, weight = 2, item = foo},
               #erlsack_item{value = 2, weight = 2, item = bar}
              ],
              {0, []}
             }
            ],
    F = fun({Title, Capacity, Items, Expected}) ->
                Actual = erlsack:fill(Capacity, Items),
                [
                 {Title, ?_assertEqual(Expected, Actual)}
                ]
        end,
    lists:map(F, Cases).
