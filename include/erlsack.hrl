-record(erlsack_item, {
          value :: number(),
          weight :: non_neg_integer(),
          tag = undefined :: erlsack_tag() | undefined
         }).

-type erlsack_tag() :: atom().

-type erlsack_item() :: #erlsack_item{}.

-type erlsack_result() :: {
        TotalValue :: number(),
        Items :: [erlsack_item()]
       }.
