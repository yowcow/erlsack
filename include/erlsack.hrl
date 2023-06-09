-record(erlsack_item, {
          value :: pos_integer(),
          weight :: pos_integer(),
          item :: term()
         }).

-type erlsack_item() :: #erlsack_item{}.
