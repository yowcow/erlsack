-module(erlsack_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/erlsack.hrl").

fill_test_() ->
    Cases = [
             {
              "capacity = 0",
              0,
              [
               #erlsack_item{value = 1, weight = 1}
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
               #erlsack_item{value = 1, weight = 1}
              ],
              {1, [
                   #erlsack_item{value = 1, weight = 1}
                  ]}
             },
             {
              "1 item when capacity < weight",
              1,
              [
               #erlsack_item{value = 1, weight = 2}
              ],
              {0, []}
             },
             {
              "total weight within capacity",
              10,
              [
               #erlsack_item{value = 2, weight = 1, tag = hoge},
               #erlsack_item{value = 5, weight = 4, tag = fuga},
               #erlsack_item{value = 6, weight = 5, tag = foo}
              ],
              {13, [
                    #erlsack_item{value = 2, weight = 1, tag = hoge},
                    #erlsack_item{value = 5, weight = 4, tag = fuga},
                    #erlsack_item{value = 6, weight = 5, tag = foo}
                   ]}
             },
             {
              "total weight over capacity (1)",
              6,
              [
               #erlsack_item{value = 5, weight = 4, tag = hoge},
               #erlsack_item{value = 4, weight = 3, tag = fuga},
               #erlsack_item{value = 3, weight = 2, tag = foo},
               #erlsack_item{value = 2, weight = 1, tag = bar}
              ],
              {9, [
                   #erlsack_item{value = 2, weight = 1, tag = bar},
                   #erlsack_item{value = 3, weight = 2, tag = foo},
                   #erlsack_item{value = 4, weight = 3, tag = fuga}
                  ]}
             },
             {
              "item count over capacity 6 (2)",
              6,
              [
               #erlsack_item{value = 7, weight = 4, tag = hoge},
               #erlsack_item{value = 4, weight = 3, tag = fuga},
               #erlsack_item{value = 3, weight = 2, tag = foo},
               #erlsack_item{value = 2, weight = 1, tag = bar}
              ],
              {10, [
                    #erlsack_item{value = 3, weight = 2, tag = foo},
                    #erlsack_item{value = 7, weight = 4, tag = hoge}
                   ]}
             },
             {
              "no item within capacity",
              1,
              [
               #erlsack_item{value = 7, weight = 4, tag = hoge},
               #erlsack_item{value = 4, weight = 3, tag = fuga},
               #erlsack_item{value = 3, weight = 2, tag = foo},
               #erlsack_item{value = 2, weight = 2, tag = bar}
              ],
              {0, []}
             },
             {
              "items with the same tag only",
              10,
              [
               #erlsack_item{value = 2, weight = 1, tag = hoge},
               #erlsack_item{value = 5, weight = 4, tag = hoge},
               #erlsack_item{value = 6, weight = 5, tag = hoge}
              ],
              {6, [
                    #erlsack_item{value = 6, weight = 5, tag = hoge}
                   ]}
             },
             {
              "items with the duplicated tags",
              10,
              [
               #erlsack_item{value = 2, weight = 1, tag = foo},
               #erlsack_item{value = 4, weight = 3, tag = fuga},
               #erlsack_item{value = 5, weight = 4, tag = hoge},
               #erlsack_item{value = 6, weight = 5, tag = hoge}
              ],
              {12, [
                    #erlsack_item{value = 2, weight = 1, tag = foo},
                    #erlsack_item{value = 4, weight = 3, tag = fuga},
                    #erlsack_item{value = 6, weight = 5, tag = hoge}
                   ]}
             },
             {
              "items with the duplicated tags + max seq",
              7,
              [
               #erlsack_item{value = 2, weight = 1, tag = foo},
               #erlsack_item{value = 4, weight = 3, tag = fuga},
               #erlsack_item{value = 5, weight = 4, tag = hoge},
               #erlsack_item{value = 6, weight = 5, tag = hoge}
              ],
              2,
              {9, [
                    #erlsack_item{value = 4, weight = 3, tag = fuga},
                    #erlsack_item{value = 5, weight = 4, tag = hoge}
                   ]}
             }
            ],
    F = fun({Title, Capacity, Items, Expected}) ->
                Actual = erlsack:fill(Capacity, Items),
                [
                 {Title, ?_assertEqual(Expected, Actual)}
                ];
           ({Title, Capacity, Items, MaxSeq, Expected}) ->
                Actual = erlsack:fill(Capacity, Items, MaxSeq),
                [
                 {Title, ?_assertEqual(Expected, Actual)}
                ]
        end,
    lists:map(F, Cases).
