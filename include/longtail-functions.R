#
# Oscar Celma. http://mtg.upf.edu/~ocelma/PhD
# Implementation of the Long Tail model
#

#
# F(x) . The long-tail model
# Reference: http://www.firstmonday.org/issues/issue12_5/kilkki/
# Params: 
#	rank_index : 	Rank (either an integer or a list)
#	n50 :		the number of objects that cover half of the whole volume (see next function) 
#	beta:		total volume
#	alfa:		the factor that defines the form of the function
F <- function (rank_index, n50, beta=1.0, alfa=0.49)
{
	if ( alfa > 1.0 ) alfa = 1.0 
        xx = as.numeric( rank_index ) # as.numeric() prevents overflow
        Fx = beta / ( (n50/xx)^alfa + 1 )
        Fx * 100
}

#
# N50
# Reference: http://www.firstmonday.org/issues/issue12_5/kilkki/
# Get the rank index for the 50% objects
#
N50 <- function(cum_pcnt, error = 0.1)
{
	GetRank(cum_pcnt, 50, error)
}

#
# GetRank
# Returns the rank index of the 'cumulative percentage', for the given 'percentage' (with an error 'error')
# Params: 
#	cum_pcnt:	cumulative percentage
#	pcnt :		percentage
#	error:		error distance (Known bug: if error value is too low, the function can loop forever!)
#
GetRank <- function(cum_pcnt, pcnt = 50, error = 0.01)
{
	max = length(cum_pcnt)
	min = 1

	# Dicotomic search to find the index position (the cum_pcnt array is ordered descendant)
	i = round( (max-min)/2 ) 
	cum_value = cum_pcnt[i]
	steps = 0
	while ( i > 1 && i < length(cum_pcnt) && abs(cum_value - pcnt) > error )
	{
		if ( cum_value > pcnt ) max = i else min = i
		i = round( (max-min)/2 + min ) 
		cum_value <- cum_pcnt[i]

		steps = steps + 1
		if ( steps > log(length(cum_pcnt)) )
		{
			error = error * 5
			steps = 0
		}
	}
	i # Returns the index 'i' that contains the 'pcnt' of the collection objects in the 'cum_pcnt' array
}
