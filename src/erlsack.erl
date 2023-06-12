-module(erlsack).

-export([
         fill/2,
         fill/3
        ]).

-include("include/erlsack.hrl").

-spec fill(
        Capacity :: non_neg_integer(),
        Items :: [erlsack_item()]
       ) -> erlsack_result().
fill(Capacity, Items) ->
    fill(Capacity, Items, undefined).

-spec fill(
        Capacity :: non_neg_integer(),
        Items :: [erlsack_item()],
        MaxSeq :: non_neg_integer() | undefined
       ) -> erlsack_result().
fill(0, _, _) ->
    {0, []};
fill(_, [], _) ->
    {0, []};
fill(_, _, 0)->
    {0, []};
fill(Capacity, Items, MaxSeq) ->
    {Result, _} = get_sack(
                    length(Items)-1,
                    Capacity,
                    array:from_list(
                      lists:sort(
                        fun(#erlsack_item{weight = A}, #erlsack_item{weight = B}) -> A > B end,
                        Items
                       )),
                    MaxSeq),
    Result.

-type internal_erlsack_result() :: {
        erlsack_result(),
        sets:set(erlsack_tag())
       }.

-spec get_sack(
        I :: integer(),
        J :: integer(),
        M :: array:array(erlsack_item()),
        MaxSeq :: non_neg_integer() | undefined
       ) -> internal_erlsack_result().
get_sack(I, J, _, _) when I < 0 orelse J =< 0 ->
    {{0, []}, sets:new()};
get_sack(I, J, M, MaxSeq) ->
    #erlsack_item{
       value = _V,
       weight = W,
       tag = _T
      } = Item = array:get(I, M),
    C0 = get_sack(I-1, J, M, MaxSeq),
    case W > J of
        true ->
            C0;
        _ ->
            {_, Set1} = C1 = get_sack(I-1, J-W, M, MaxSeq),
            case MaxSeq of
                undefined ->
                    %% if MaxSeq is NOT specified, just determine if Item should be added to the result
                    determine_item(Item, C0, C1);
                _ ->
                    %% if MaxSeq is specified, check if current count of items is less than MaxSeq
                    case sets:size(Set1) >= MaxSeq of
                        true ->
                            C0;
                        _ ->
                            determine_item(Item, C0, C1)
                    end
            end
    end.

-spec determine_item(
        erlsack_item(),
        internal_erlsack_result(),
        internal_erlsack_result() 
       ) -> internal_erlsack_result().
determine_item(
  #erlsack_item{value = V, tag = T} = Item,
  {{S0, _}, _} = C0,
  {{S1, Items1}, Set1}
 ) ->
    %% check if there's a room for the item
    case (S0 > (S1 + V)) of
        true ->
            C0;
        _ ->
            %% check if the item's tag has NOT been added to the result
            case sets:is_element(T, Set1) of
                true ->
                    C0;
                _ ->
                    {{S1 + V, [Item | Items1]},
                     sets:add_element(T, Set1)}
            end
    end.
