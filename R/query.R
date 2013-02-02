# ---------------------------------------------------
# Copyright 2012 Taras Zakharko
# 
# This file is part of the ToolboxSearch R package.
# 
# ToolboxSearch is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 2 of the License, or (at your option)
# any later version.
# 
# ToolboxSearch is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.
# ---------------------------------------------------


###########################
#
#  Corpus query interface
#
###########################

corpus.query <- function(corpus, query)
{
	if(!is.character(query)) stop("query must be a character string!\n")
	if (length.corpus(corpus)==0) return(index.corpus(integer(0), level=levels(attr(corpus, "format.descriptor"))[1]))
		
	time <- system.time({
		q <- postprocess.query(.Call("parse", query), corpus)
		q <- eval.query(q, corpus)
		res <- unclass(q$ref.table$obj[[unclass(q$qobj)]])
	})	
	
	# print the time only if called from the console directly
	#if(identical(.GlobalEnv, parent.frame())) 
	#cat("Query done in", time[3], "seconds\n")
	
	index.corpus(res, level = q$ref.table$level[[unclass(q$qobj)]])
}


"%%.language.corpus" <- function(corpus, query) corpus.query(corpus, query)


###########################
#
#  Corpus query preprocessing
#
###########################

dnf <- function(x)
{	
	x<- switch(class(x),
		node.OR = {
			structure(c(dnf(x[[1]]), dnf(x[[2]])), class="dnf")
		},
		node.AND = {
			x <- lapply(dnf(x[[1]]), function(a)
					lapply(dnf(x[[2]]), function(b)
						c(a, b)))
			
			x <- do.call(c, x)
			structure(x, class="dnf")
		},
		structure(list(list(x)), class="dnf")
	)
	
	
	x
}

postprocess.query <- function(q, corpus)
{
	typ <- sapply(q[[2]], function(x) switch(class(x),
	 	node.REFERENCE.OBJ = "obj",
	 	node.TOPO.PATTERN = "topo"))
	
			
	obj <- q[[2]]
	
	
	
	obj[typ == "obj"] <- lapply(obj[typ == "obj"], function(ref)
	{
		ref[[1]] <- dnf(ref[[1]])
		ref
	})


	t <- data.frame(typ = typ)
	t$obj <- obj
	
	
	# find object dependences
	objdep <- lapply(1:nrow(t), function(ref.i) 
	if(t$typ[ref.i] == 'var') 
	{
		for(b in attr(q[[1]], "where"))
			if(b[[1]] == ref.i) return(b[[2]])
			
		NA
	} else
	if(t$typ[ref.i] == 'obj') 
	{
		
		members <- do.call(c, t$obj[[ref.i]][[1]])
		
		if(length(unlist(members))==0) return(integer(0L)) else
		as.integer(na.omit(sapply(members, function(x) 
		{
			if(class(x) == "node.NOT") x<-x[[1]]
			
			switch(class(x),
				node.EVAL.PREDICATE = NA,
				node.EVAL.CONTAINS.OBJECT = x[[1]],
				node.EVAL.CONTAINS.TOPO = x[[1]])

		})))
	} else
	if(t$typ[ref.i] == 'topo') 
	{
		members <- unlist(t$obj[ref.i])
		members[members>0]
	})
	
	t$level <- ""
	t$level[t$typ == "obj"] <- sapply(t$obj[t$typ == "obj"], attr, "level")
	t$level[t$typ == "topo"] <- sapply(which(t$typ == "topo"), function(topo.i)
	{
		dobj <- objdep[[topo.i]]
		
		# if its not bound to a level, then its the lowest-level (atom-count)
		if(length(dobj) == 0) return(names(corpus$tok.table)[length(corpus$tok.table)])
		
		lvl <- unique(t$level[dobj])
		
		if(length(lvl) > 1)
			stop("Cannot mix different levels in one pattern (", paste(lvl, collapse = ", "), ")")

		lvl
	})
	
		
	full.dep <- function(deps)
	{
		if(length(deps) == 0) return(deps)
		if(all(is.na(deps))) return(deps)
		
		deps1 <- setdiff(na.omit(unique(unlist(objdep[deps[t$typ[deps] %in%  c("topo", "obj")]]))), deps)		
		deps1 <- deps1[deps1>0]
		
		if(length(deps1) == 0) return(deps)
		
		full.dep(c(deps, deps1))
	}
				
	
	t$dep.on <- lapply(objdep, full.dep)
	

	
	
	q[[2]] <- t
	
	q <- q[-4]
	
	names(q) <- c("qobj", "ref.table", "pred.table")

	q
}

###########################
#
#  Corpus query preprocessing
#
###########################

eval.query <- function(query, corpus)
{	
	corpus.fmt <- attr(corpus, "format.descriptor")
		
	# which references must be evaluated?
	find.to.eval <- function(dep)
	{
		new.dep <- setdiff(unique(unlist(query$ref.table$dep.on[dep])), dep)
		
		if(length(new.dep) == 0) return(dep)
		
		find.to.eval(c(new.dep, dep))
	}
	
	to.eval <- find.to.eval(unclass(query$qobj))
	
	e <- new.env()
	query$ref.table$eval.state <- "discarded"
	query$ref.table$eval.state[to.eval] <- rep("pending", length(to.eval)) 
	
	e$refs <- query$ref.table
	
	
	while(T)
	{
		# find a ref which is pending and which its reqs non-pending
		candidates <- which(e$refs$eval.state == "pending")
		
		if(length(candidates) == 0) break
		
		score <- sapply(candidates, function(x) {
			deps <- e$refs$dep.on[[x]]
			sum(e$refs$eval.state[deps] == "pending")
		})
		
		
		o.i <- which.min(score)
		score <- score[o.i]
		o.i <- candidates[o.i]
		
		obj <- e$refs$obj[[o.i]]
		typ <- e$refs$typ[[o.i]]
		lvl <- e$refs$level[[o.i]]
				
		if(typ == "obj")
		{
			condition <- obj[[1]]
			
						
			eval.conj <- function(conj)
			{
				eval.item <- function(x)
				switch(class(x),
				node.EVAL.PREDICATE = {
					pred <- query$pred.table[[unclass(x)]]
			
					v <- attr(pred, "var")
			
					if(!v %in% corpus.fmt[lvl])
						stop("Unknown attribute ", v, " of level ", lvl)
			
					op <- attr(pred, "op")
					arg <- attr(pred, "arg1")
					
					
					switch(op,
						'==' = corpus$tiers[[v]]  %in% arg,
						'!=' = !(corpus$tiers[[v]] %in% arg),	
						'=~' = grepl(arg, corpus$tiers[[v]], perl=T, ignore.case=T),
						'!~' = !grepl(arg, corpus$tiers[[v]], perl=T, ignore.case=T)
					)
					
				},
				node.EVAL.CONTAINS.OBJECT = {
					ref.o <- unclass(x)
			
					lvl.o <- e$refs$level[[ref.o]]
					co <- which(c(lvl, lvl.o) %in% levels(corpus.fmt))
					if(co[2] <= co[1]) return(F)

					if(e$refs$eval.state[ref.o] != "done") return(NULL)
			
					ref.id <- e$refs$obj[[ref.o]]

					
					mm <- corpus$tok.table[[lvl.o]] %in% ref.id
						
					r <- logical(length(corpus$tiers[[corpus.fmt[lvl][1]]]))
					r[unique(corpus$tok.table[[lvl]][mm])] <- T
					
					r
				},
				node.NOT = {
					x <- eval.item(x[[1]])
					if(is.null(x)) return(NULL)
					!x
				},
				node.EVAL.CONTAINS.TOPO = {
					# leave it at partial!
					return(NULL)
				},
				F
				)
				
				
				conj.r <- lapply(conj, eval.item)
				
				computed <- sapply(conj.r, length) > 0
				
				
				if(!any(computed)) return(conj)
				
				conj <- conj[!computed]
				conj.r <- conj.r[computed]
				
				computed <- Reduce(get("&"), conj.r[-1], conj.r[[1]])
				
				
				
				if(!any(computed)) return(NULL) else return(structure(conj, initial=computed))
			}
			
			if(length(unlist(condition))==0)
			{
				# the condition is empty, this is the same as any
				computed <- seq_along(corpus$tiers[[corpus.fmt[lvl][1]]])
				
				obj <- structure(as.integer(computed), class="node.COMPUTED.REF")
				
				e$refs$obj[[o.i]] <- obj
				e$refs$eval.state[o.i] <- "done"
				
				next
			}
			
			
			disj <- lapply(condition, eval.conj)
			
			disj <- disj[!sapply(disj, is.null)]
			
			computed <- sapply(disj, length) == 0
						
			disj.r <- lapply(disj[computed], attr, "initial")
			disj <- disj[!computed]
						
			computed <- if(length(disj.r) == 0) NULL else Reduce(get("|"), disj.r[-1], disj.r[[1]])
			
			# each dnf term is fully computed by now
			if(length(disj) == 0)
			{
				computed <- if(length(computed) == 0)
				integer(0)
				else which(computed)

				obj <- structure(as.integer(computed), class="node.COMPUTED.REF")
				
				e$refs$obj[[o.i]] <- obj
				e$refs$eval.state[o.i] <- "done"
			} else
			{
				computed <- if(length(computed) == 0) integer(0) else which(computed)
				
				disj <- lapply(disj, function(x) {
					o <- attr(x, "initial")
					
					o <- if(length(o) == 0)
					{
						if(all(o))  seq_along(corpus$tiers[[corpus.fmt[lvl][1]]]) else integer(0)
					} else which(o)
					
					attr(x, "initial") <- o
					
					x
				})
								
				# if we are here, it means that there are still topos to compute!
				# we make use of that
				disj <- lapply(disj, function(conj)
				{
					initial <- attr(conj, "initial")
					if(length(initial) == 0) initial = seq_along(corpus$tiers[[corpus.fmt[lvl][1]]])
										
					# do the NFA matching
					for(conj.i in conj)
					{
						if(attr(conj.i,"class") == "node.NOT")
						{
							invert <- T
							conj.i <- conj.i[[1]]
						} else
						{
							invert <- F
						}						
						
						topo.i <- as.integer(conj.i)
						
						# TODO Special cases with cross-level topos (item count topo)
						lvl.o <- e$refs$level[[topo.i]]
						
						co <- which(c(lvl, lvl.o) %in% levels(corpus.fmt))
						if(co[2] <= co[1]) return(F) # level mismatch!
						
						nfa <- e$refs$obj[[topo.i]]
						
						
						candidates <- .Call("_fastsplit", corpus$tok.table[[lvl.o]], corpus$tok.table[[lvl]], initial)

						candidates <- fast_match_nfa(candidates, nfa, e$refs$obj)
							
						if (invert)
							candidates <- !candidates
						
						initial <- initial[candidates]

						
						if(length(initial) == 0) return(integer(0))
						
					}
					
					initial
				})
				
				
				computed <- sort(unique(unlist(disj)))
				
				obj <- structure(as.integer(computed), class="node.COMPUTED.REF")
				
				e$refs$obj[[o.i]] <- obj
				e$refs$eval.state[o.i] <- "done"
			}						
		} else
		if(typ == "var")
		{
			# compute the initial
			bind.i <- e$refs$binding[o.i]
			bind.obj <- e$refs$obj[[bind.i]]
			
			
			if(e$refs$eval.state[bind.i] == "done")
			{
				attr(obj, "initial") <- unclass(bind.obj)
				e$refs$obj[[o.i]]  <- obj
				e$refs$eval.state[o.i] <- "predicted"
			} else
			{
				initial <- unique(unlist(lapply(bind.obj, attr, "partial")))
								
				attr(obj, "initial") <- as.integer(initial)
				e$refs$obj[[o.i]]  <- obj
				e$refs$eval.state[o.i] <- "predicted"
			}
		} else
		if(typ == "topo")
		{
			topo <- e$refs$obj[[o.i]]
			topo <- topo_to_nfa(topo)
			
			e$refs$obj[[o.i]] <- topo
			e$refs$eval.state[o.i] <- "constructed"
		}
		
	}
	
	query$ref.table <- e$refs
	
	query
}

