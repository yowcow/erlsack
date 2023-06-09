-module(erlsack).

-export([
         fill/2
        ]).

-include("include/erlsack.hrl").

-spec fill(
        Capacity :: non_neg_integer(),
        Items :: [erlsack_item()]
       ) -> {number(), [erlsack_item()]}.
fill(0, _) ->
    {0, []};
fill(_, []) ->
    {0, []};
fill(Capacity, Items) ->
    fill(
      Capacity,
      lists:sort(
        fun(#erlsack_item{weight = A}, #erlsack_item{weight = B}) -> A > B end,
        Items
       ),
      lists:foldl(
        fun(#erlsack_item{weight = W}, Acc) -> Acc + W end,
        0,
        Items
       )).

-spec fill(
        Capacity :: non_neg_integer(),
        Items :: [erlsack_item()],
        TotalWeight :: non_neg_integer()
       ) -> {number(), [erlsack_item()]}.
fill(Capacity, Items, TotalWeight) when TotalWeight =< Capacity ->
    {lists:foldl(
       fun(#erlsack_item{value = V}, Acc) -> Acc + V end,
       0,
       Items
      ), Items};
fill(Capacity, Items, _) ->
    ItemCount = length(Items),
    get_sack(
      ItemCount-1,
      Capacity,
      array:from_list(Items)
     ).

-spec get_sack(
        I :: integer(),
        J :: integer(),
        M :: array:array(erlsack_item())
       ) -> {number(), [erlsack_item()]}.
get_sack(I, J, _) when I < 0 orelse J =< 0 ->
    {0, []};
get_sack(I, J, M) ->
    #erlsack_item{value = V, weight = W} = Item = array:get(I, M),
    {S0, _} = C0 = get_sack(I-1, J, M),
    case W > J of
        true ->
            C0;
        _ ->
            {S1, Items1} = get_sack(I-1, J-W, M),
            case (S0 > (S1 + V)) of
                true ->
                    C0;
                _ ->
                    {S1 + V, [Item | Items1]}
            end
    end.
