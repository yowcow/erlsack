-record(erlsack_item, {
          value :: number(),
          weight :: non_neg_integer(),
          item :: term()
         }).

-type erlsack_item() :: #erlsack_item{}.
