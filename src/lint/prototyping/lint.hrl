-ifndef(LINT_HRL).
-define(LINT_HRL, true).

-record(node, {
               name,
               row,
               value,
               type,
               children = []
              }).
-record(equation, {
                   origin_node_name,
                   origin_row,
                   row,
                   value,
                   type,
                   user_defined
                  }).

-endif.
