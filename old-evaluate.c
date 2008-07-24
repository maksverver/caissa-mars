INLINE value_t evaluate(int depth)
{
#ifndef SIMPLE_EVAL
	move_t move_list[MAX_MOVES];
	int moves;
#ifdef RETURN_CHECK
	BOOL checked = FALSE;
#endif
#endif
	value_t val = 0;

#	ifdef DEBUG
		++total_situations;
#	endif

	#	ifndef SIMPLE_EVAL
			moves = generate_moves(move_list
	#ifdef RETURN_CHECK
				, &checked
	#endif
				);

			assert(checked != 123);

			/* Check for end of game */
			if(moves == 0)
				return (checked ? depth - VAL_CHECKMATE: 0);

			/* Value the availability of moves
				Score: 1 .. 56
			*/
		val += moves;
#	endif

	/* Give bonuses/penalties to queen-aligned bishops
		Score: -3 .. 0
	*/
	if(
		(GAME.current.pos[BISHOP] != POS_NULL) &&
		(POS_ALIGN(GAME.current.pos[BISHOP]) != POS_ALIGN(GAME.other.pos[QUEEN]))
	)
		val -= 2;
	if(
		(GAME.other.pos[BISHOP] != POS_NULL) &&
		(POS_ALIGN(GAME.other.pos[BISHOP]) != POS_ALIGN(GAME.current.pos[QUEEN]))
	)
		val += 2;

	/* Give bonuses/penalties for free fields around the queen
		Score: 0 .. 8
	*/
	{
		position_t *fields, dst;

#		ifndef SIMPLE_EVAL	
/*		if(!may_remove_tile(GAME.current.pos[QUEEN]))
			val -= 10;
		else*/
#endif
		for(fields = adjacent_fields[GAME.current.pos[QUEEN]];
			(dst = *fields) != POS_NULL; ++fields)
			if((BIT(dst) & GAME.tiles) &&
				(GAME.current.pos[ROOK]   != dst) &&
				(GAME.current.pos[KNIGHT] != dst) &&
				(GAME.current.pos[BISHOP] != dst))
					val += 1;
	
#		ifndef SIMPLE_EVAL	
/*
		if(!may_remove_tile(GAME.other.pos[QUEEN]))
			val += 10;
		else*/
#endif
		for(fields = adjacent_fields[GAME.other.pos[QUEEN]];
			(dst = *fields) != POS_NULL; ++fields)
			if((BIT(dst) & GAME.tiles) &&
				(GAME.other.pos[ROOK]   != dst) &&
				(GAME.other.pos[KNIGHT] != dst) &&
				(GAME.other.pos[BISHOP] != dst))
					val -= 1;
	}

	/* Give points for pieces on the board (material value)
		Score: 0 .. 195
	*/
	{
		const int piece_value[4] = { 45, 90, 60, 0 };
		piece_t piece;
		for(piece = 0; piece < PIECES; ++piece)
		{
			if(GAME.current.pos[piece] != POS_NULL)
				val += piece_value[piece];
			if(GAME.other.pos[piece] != POS_NULL)
				val -= piece_value[piece];
		} 

	}

	assert(checked != 123);
	if(checked)
		val -= 8;

	return val;
}
