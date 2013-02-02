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
#  nondeterministic finite state automaton query preprocessing
#
###########################
remap.states <- function(nfa, map)
{
	nfa$states <- lapply(nfa$states, function(state)
	{
		if(nrow(state$tr) == 0) return(state)
		
		state$tr <- do.call(rbind, lapply(1:nrow(state$tr), function(row)
					{
						row <- state$tr[row, ]
						if(row$state <= 0) return(row)
						
						row$state <- map[[row$state]]
						row
					}))	
		state
	})
	
	nfa$start <- unlist(map[nfa$start])
	nfa$end <- unlist(map[nfa$end])
	
	nfa
}

# create an empty state (this is to initialize the machine)
nfa_start_state <- function(tnum)
	structure(list(states = list(structure(list(tr=data.frame(match=integer(0), state=integer(0))), class = "nfastate")), start  = 1, end = 1, incoming = integer(0)), class="nfa")

# create a match
nfa_match_state <- function(match, tnum)
	structure(list(states = list(structure(list(tr=data.frame(match=integer(0), state=integer(0))), class = "nfastate")), start  = 1, end = 1, incoming = match), class="nfa")


nfa_empty <- function()
	structure(list(states = list(structure(list(tr=data.frame(match=integer(0), state=integer(0))), class = "nfastate")), start  = 1, end = 1), class="nfa")


nfa_match <- function(match)
{
	s0 <- structure(list(tr = data.frame(match = match, state = 2)), class = "nfastate")
	s1 <- structure(list(tr = data.frame(match = integer(0), state = integer(0))), class = "nfastate")
	
	structure(list(states = list(s0, s1), start  = 1, end = 2), class="nfa")
}

# create a bad match
nfa_match_complement <- function(match)
{
	s0 <- structure(list(tr = data.frame(match = c(match, -match), state = c(0L, 2L))), class = "nfastate")
	s2 <- structure(list(tr = data.frame(match = integer(0), state = integer(0))), class = "nfastate")
	
	structure(list(states = list(s0, s2), start  = 1, end = 2), class="nfa")
}


# create a repetition
nfa_repeat <- function(nfa, min, max)
{
	# we do the max part first
	if(max == Inf)
	{
		nfa.max <- nfa_kleene(nfa)
	} else
	if(max > min)
	{
		nfa.max <- nfa
		
		mm <- max - min
		final <- nfa.max$end
		
		if(mm>1)
		for(i in 1:(mm-1))
		{
			nfa.max <- nfa_sequence(nfa.max, nfa)
			final <- c(final, nfa.max$end)
		}
		
		nfa.max$end <- unique(c(final, nfa.max$start))
	}
		
	if(min == 0) return(nfa.max)
	
	# make a sequence min times
	nfa.min <- if(min == 1) nfa else Reduce(nfa_sequence, replicate(min-1, nfa, simplify=F), nfa)
		   
	if(min == max) return(nfa.min)

	nfa_sequence(nfa.min, nfa.max)
}

nfa_sequence <- function(nfa1, nfa2)
{
	# e-transition
	# collapse the end states of nfa1 and start states of nfa2
	s <- nfa2$start
	state.remap <- integer(length(nfa2$states))
	state.remap[-s] <- length(nfa1$states) + seq_along(state.remap[-s])
	state.remap <- as.list(state.remap)
	state.remap[s] <- replicate(length(s), nfa1$end, simplify=F)
		
		
				 
	nfa2 <- remap.states(nfa2, state.remap)
	

	ss <- nfa2$states[s]

				
	merge.states <- function(a, b) {
		a$tr <- unique(rbind(a$tr, b$tr))
				
		a
	}
				
	ss <- Reduce(merge.states, ss[-1], ss[[1]])
			
			
		
			
	for(i in nfa1$end)
		nfa1$states[[i]] <- merge.states(nfa1$states[[i]], ss)

			
		nfa2$states <- 	nfa2$states[-s]						
	
		
	nfa1$states <- c(nfa1$states, nfa2$states)
	
	

	nfa1$end <- nfa2$end	
	
	nfa1
}


nfa_kleene <- function(nfa)
{
	
	o.nfa <- nfa
	
	e <- setdiff(nfa$end, nfa$start)
	state.remap <- as.list(seq_along(nfa$states))
	state.remap[e] <- replicate(length(e), nfa$start, simplify=F)
	nfa <- remap.states(nfa, state.remap)

	ss <- nfa$states[e]

				
	merge.states <- function(a, b) {
		a$tr <- unique(rbind(a$tr, b$tr))
				
		a
	} 
				
	ss <- Reduce(merge.states, ss[-1], ss[[1]])
			
	for(i in nfa$start)
		nfa$states[[i]] <- merge.states(nfa$states[[i]], ss)
	
	nfa$states <- nfa$states[-e]						
		
	nfa$end <- nfa$start	
	
	nfa <- nfa_sequence(o.nfa, nfa)
	nfa$end <- unique(c(nfa$end, nfa$start))
	
	nfa
}


fast_match_nfa <- function(tokens, nfa, match.table)
{
	leftbr <- attr(nfa, "leftbr")
	if(is.null(leftbr)) leftbr <- F

	rightbr <- attr(nfa, "rightbr")
	if(is.null(rightbr)) rightbr <- F
	
	
	# test the pathologic machine (accept on init)
	if(!leftbr)
		if(length(intersect(nfa$start, nfa$end))>0) return(rep(T, length(tokens)))
		
	nfa$states  <- lapply(nfa$states, function(x)  {x$tr$match <- as.integer(x$tr$match); x$tr$state <- as.integer(x$tr$state); x})
	
	
	.Call("match_nfa", tokens, nfa$states, as.integer(nfa$start), as.integer(nfa$end), match.table, leftbr, rightbr)
}

topo_to_nfa <- function(x)
switch(class(x),
	node.TOPO.MATCH.REPEAT = {
		reps = attr(x, "range")
		if(is.na(reps[2])) reps[2] <- Inf
		nfa_repeat(topo_to_nfa(x[[1]]), reps[1], reps[2])
	},
	node.TOPO.MATCH = {
		if(x[[1]] == -1) return(nfa_match(0))
		if(x[[1]] == -2) return(NULL)
		
		nfa_match(x[[1]])
	},
	node.TOPO.MATCH.INV = nfa_match_complement(x[[1]]),
	node.TOPO.PATTERN = {
		x <- lapply(x, topo_to_nfa)

		leftbr <- F
		if(is.null(x[[1]]))
		{
			leftbr <- T
			x <- x[-1]
		}
		rightbr <- F
		if(is.null(x[[length(x)]]))
		{
			rightbr <- T
			x <- x[-length(x)]
		}
		
		if(any(sapply(x, is.null))) stop("pattern boundary cannot occur in the middle of the pattern!")
		
		nfa <- Reduce(nfa_sequence, x[-1], x[[1]])
		attr(nfa, "leftbr") <- leftbr
		attr(nfa, "rightbr") <- rightbr
		
		nfa
		
	}
)