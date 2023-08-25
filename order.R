

new_order_lst <- function() {
  structure(list(),
            class = c("order_lst"))
}

new_order <- function() {
  structure(list(),
            class = c("order", "condition"))
}

ord_order <- function() {
  ord <- new_order()
  signalCondition(ord)
  # cat("função continua..\n")
  ord
}

order_lst <- function() {
  restart <- findRestart("orders_query")
  if (is.null(restart)) {
    return(new_order_lst())
  }
  restart$handler()
}

library(glue)
internal_order_list <- new_order_lst()

res <- withRestarts(
  withCallingHandlers({
      ord_order()
      cat(glue("Nº de ordens: {length(order_lst())}"), "\n")
      ord_order()
      cat(glue("Nº de ordens: {length(order_lst())}"), "\n")
      ord_order()
      cat(glue("Nº de ordens: {length(order_lst())}"), "\n")
      order_lst()
    },
    order = function(o) {
      internal_order_list <<- append(internal_order_list, list(o))
    }),
  orders_query = function() internal_order_list)
  
