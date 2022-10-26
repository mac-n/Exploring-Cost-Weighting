#best first search from FSelector Package
#with a hacky but handy global variable added to record all the outputs from the best first search
#these lines here:
#row=c(paste(attrs,collapse=","),eval.fun(attrs))
#therow<-rbind(therow,row)
#you want to initialise therow outside the function as follows:

therow<-matrix(ncol=2,nrow=0)

best.first.search <- function(attributes, eval.fun, max.backtracks = 5) {
  if(length(attributes) == 0)
    stop("Attributes not specified")
  
  eval.fun = match.fun(eval.fun)
  
  attach_children <- function(states, best) {
    parent_state = states$attrs[best$idx,]
    children = create.children(parent_state, "forward", omit.func = function(...) {
      length(find.subset(states$attrs, ...)) > 0
    })
    children_len = ifelse(is.null(children), 0, dim(children)[1])
    if(children_len > 0) {
      states$attrs = rbind(states$attrs, children)
      states$open = c(states$open, rep(TRUE, children_len))
      states$results = c(states$results, rep(NA, children_len))
    }
    return(states)
  }
  
  states = list(
    attrs = diag(length(attributes)),
    open = rep(TRUE, length(attributes)),
    results = rep(NA, length(attributes))
  )
  colnames(states$attrs) = attributes
  
  best = list(
    result = -Inf,
    idx = NULL
  )

  repeat {
    # calculate merit for every open and not evaluated subset of attributes
    rows_to_eval = states$open & is.na(states$results)
    if(any(rows_to_eval)) {
      states$results[rows_to_eval] = 
        apply(states$attrs[rows_to_eval,, drop=FALSE], 1, function(vec) {
          attrs = attributes[as.logical(vec)]
          #message(paste(attrs,collapse=","),";",eval.fun(attrs))
          row=c(paste(attrs,collapse=","),eval.fun(attrs))
          therow<-rbind(therow,row)
          return(eval.fun(attrs))
        })
    }
    #find best
    new_best = find.best(states$results, states$open)
    
    #check if better
    if(is.null(new_best$result))
      break()
    states$open[new_best$idx] = FALSE
    if(new_best$result > best$result) {
      best = new_best
      
      states = attach_children(states, best)
    } else {
      if(max.backtracks > 0) {
        max.backtracks = max.backtracks - 1
      } else
        break
    }
  }
 # message (attributes[as.logical(states$attrs[best$idx, ])])
  #message("hello")
  return(attributes[as.logical(states$attrs[best$idx, ])])
  
}