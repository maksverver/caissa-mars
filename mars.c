#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <time.h>

#ifdef WIN32
#	include <sys/timeb.h>
#else
#	include <sys/time.h>
#endif


#ifdef WIN32
typedef __int64 int64;
#else
typedef long long int int64;
#endif

#ifndef BOOL
typedef char BOOL;
#	define TRUE		((BOOL)1)
#	define FALSE	((BOOL)0)
#endif

#ifdef WIN32
#	define INLINE __inline
#else
#	define INLINE inline
#endif

/*
	Code generation
*/

#ifdef _DEBUG
#define DEBUG				10
#endif

/*#define PRINT_BOOK*/

#define MAX_TOTAL_MOVES		(100)
#define BOOKS				3

#ifndef LOCAL_TEST

/* CONTEST CONFIGURATION */

#define PREORDER_MOVES
#define PREORDER_DEPTH		(-3)

#define CONNECTED_CACHE		(1123)

//#define RETURN_CHECK
#define PV

#define ALGORITHM
#define ALGORITHM_SEARCH

#define MIN_SEARCH_DEPTH	(4)
#define MAX_SEARCH_DEPTH	(4)
#define SEARCH_DEPTH_INC	(2)

#define TOTAL_TIME			(115000)
#define EXPECTED_BRANCHING_FACTOR (75)
#define EXPECTED_MOVES_LEFT	(20)

#define USE_BOOK

#define QSEARCH				
#define QSEARCH_DEPTH		(24)


#else // LOCAL_TEST is defined

/* TEST CONFIGURATION */

#define PREORDER_MOVES
#define PREORDER_DEPTH		(-3)

#define CONNECTED_CACHE		(1123)

//#define RETURN_CHECK
#define PV

/*
#define TT
#define	TT_BUCKETS			(2011)	// 501 
#define	TT_MIN_DEPTH		(2)
*/

#define MIN_SEARCH_DEPTH	(6)
#define MAX_SEARCH_DEPTH	(6)
#define SEARCH_DEPTH_INC	(2)

//#define ASPIRATION_WINDOW	(25)

//#define QSEARCH				
#define QSEARCH_DEPTH		(24)

#define TOTAL_TIME			(45000)
#define EXPECTED_BRANCHING_FACTOR (75)
#define EXPECTED_MOVES_LEFT	(20)

#if 0 	/* BOOK BUILDING */
#	define BUILD_BOOK
#	define BOOK_SEARCH_DEPTH	6
#	define BOOK_DEPTH			6
#	define BOOK_HISTORY_DEPTH	1
#	define BOOK_BRANCHES		6
#	define INPUT				"opening.txt"
#	define OUTPUT				"book.txt"
#else
//#	define ERROR			"mars.log"
/*#	define INPUT			"transcript.txt"*/
#	define USE_BOOK
#	define ALGORITHM
#	define ALGORITHM_SEARCH
/*
#	define SEARCH_FOR_MATE
*/
#endif
#endif


/*
	Constant game definitions
*/

#define SIDE				(7)
#define FIELDS				(SIDE*SIDE)

/*	Maximum number of possible moves in a given situation.

		Bishop: 12
		Rook:	12
		Knight:  8
		Queen:  24 +
		       ------
			    56
*/

#define MAX_MOVES (56)

/*	The maximum length of a game in plies */
#define MAX_GAME_LENGTH (200)
/*	The length of the history record, which should be at least the
	maximum length of a game, but needs some extra entries to provide
	for recursive search at the end of the game!!!
*/
#define HISTORY_LENGTH (MAX_GAME_LENGTH + 64)

#define VAL_WORST     (-99999)
#define VAL_CHECKMATE ( 90000)

/*
	Definitions
*/

#define BISHOP				(0)
#define ROOK				(1)
#define KNIGHT				(2)
#define QUEEN				(3)
#define PIECES				(4)

#define WHITE               (0)
#define BLACK               (1)
#define COLOURS				(2)


/*
	 3  2  1
	  \ | /
	4 - * - 0
	  / | \
	 5  6  7
*/

#define DIRECTIONS			(8)




/*
	Type definitions
*/

/* The colour of a piece or player. Either WHITE or BLACK. */
typedef signed char colour_t;


/* A position; the indication of a field. A number in the range 0 to FIELDS (exclusive). */
typedef signed char position_t;


/* A piece; one of BISHOP, ROOK, KNIGHT and QUEEN. */
typedef signed char piece_t;

/* A move; consisting of an origin (the 'from' field) and a destionation (the 'to') field.
   If we wish to be able to undo moves, we need to store the original occupation as well.
*/
typedef struct move {
	piece_t		piece;
	position_t	to;
} move_t;

typedef int64 bitboard_t;

/* A union used to store the positions of all pieces of a single player. */
typedef union positions {
		position_t pos[PIECES];
		unsigned int value;
} positions_t;

/* A game state. */
typedef struct game {

	/*	Bitboard which indicates wether a given field contains a tile. */
	bitboard_t tiles;	/* 8 bytes */

	/*	Bitboard which indicates wether a given field contains a piece. */
	bitboard_t pieces;	/* 8 bytes */

	/*	The number of moves mave by both players in total (so, when both black and
		white make a move, this really counts as two moves). The least significant
		bit indicates the player that is to move next. */

	/*	For each piece, it's position on the board is recorded. This position is 
		NULL for basic pieces that have been captured. */
	positions_t
		current,		/* 4 bytes */
		other;			/* 4 bytes */

	position_t
		previous_from,	/* 1 byte */
		previous_to;	/* 1 byte */
} game_t;	/* 26 bytes of data; 32 byte sized stucture */

/* The value of a position */
typedef signed long value_t;

/* An entry for the transposition table */
typedef struct tt_bucket
{
	bitboard_t tiles;	/* 8 bytes */

	positions_t
		current,		/* 4 bytes */
		other;			/* 4 bytes */

	value_t	value;		/* 4 bytes */

	unsigned short depth;	/* 2 bytes */

	position_t
		previous_from,	/* 1 byte */
		previous_to;	/* 1 byte */

	move_t
		best_move;		/* 2 bytes */

} tt_bucket_t;	/* 26 bytes data */


/* A move, value pair. (Used for sorting moves) */
typedef struct valued_move
{
	move_t  move;
	value_t value;
} valued_move_t;


#ifdef USE_BOOK
typedef struct book
{
	const char *id;
	int depth, branches;
	long total_size;
	unsigned char *entries;
} book_t;

typedef struct bookmark
{
	const unsigned char *entry;
	long section_size;
} bookmark_t;
#endif


/* An entire gamefield, consisting of the occupation information of the fields.

   Array indices mapped to game fields, for the standard board:
      +--+--+--+--+--+--+--+ 
	7 |42|43|44|45|46|47|48|
      +--+--+--+--+--+--+--+
	6 |35|36|37|38|39|40|41|
      +--+--+--+--+--+--+--+
	5 |28|29|30|31|32|33|34|
      +--+--+--+--+--+--+--+
	4 |21|22|23|24|25|26|27|
      +--+--+--+--+--+--+--+
	3 |14|15|16|17|18|19|20|
      +--+--+--+--+--+--+--+
	2 | 7| 8| 9|10|11|12|13|
      +--+--+--+--+--+--+--+
	1 | 0| 1| 2| 3| 4| 5| 6|
      +--+--+--+--+--+--+--+
	    A  B  C  D  E  F  G


	Total number of rays:
		diagonal:				4 * 13 =  52
		horizontal/vertical:	4 *  7 =  28 +
		                                 ------
										  80
*/

#define MAX_RAYS_PER_PIECE (8)
#define MAX_FIELDS_PER_RAY (7)

/*
	Definitions for operations on variables
*/

#define ABS(x)				((x)>=0?(x):-(x))

/* Converting positions to and from x, y coordinates. */
#define XY_POS(x,y)			((position_t)((x)+SIDE*(y)))
#define POS_X(pos)			((pos)%SIDE)
#define POS_Y(pos)			((pos)/SIDE)
#define POS_ALIGN(pos)		(pos%2)

/* Switching between colours */
#define OTHER_COLOUR(c)		((colour_t)(!(c)))

#define POS_NULL			((position_t) -1)
#define POS_IS_NULL(p)		((p) == POS_NULL)
#define POS_IS_VALID(p)		(((p) >= 0) && ((p) < FIELDS))

#define PIECE_NULL			((piece_t) -1)
#define PIECE_IS_NULL(p)	((p) == PIECE_NULL)
#define PIECE_IS_VALID(p)	( (p)==QUEEN || (p)==ROOK || (p)==BISHOP || (p)==KNIGHT )

#define MOVE_IS_VALID(m)	(PIECE_IS_VALID((m).piece) && POS_IS_VALID((m).to))
#define MOVE_EQUALS(m,n)	(((m).to == (n).to) && ((m).piece == (n).piece))

#define	GAME_EQUALS(g,h) \
	(((g).current.value == (h).current.value) && \
	 ((g).other.value == (h).other.value) && \
	 ((g).tiles == (h).tiles) && \
	 ((g).previous_from == (h).previous_from) && \
	 ((g).previous_to == (h).previous_to) \
	)

#define	HASH_GAME(game,max) \
	((((unsigned int)((game).tiles % max)) ^ \
	  ((game).current.value ^ (7 * ((game).other.value))) ^ \
	  ((unsigned int)(281 * (game).previous_from + 13781 * (game).previous_to)) \
	 ) % (max))

/* Bit operations */
#define BIT(n)              (((int64)1) << (n))
#define BITS(n)             ((((int64)1) << (n)) - 1)


#define GAME				(*game)
#define CURRENT_PLAYER		((game - history) % 2)
#define MOVES_PLAYED		(game - history)


/*
	Global data
*/

game_t
	*game,						/* Current game pointer */
	history[HISTORY_LENGTH];	/* Game situations stack */

#ifdef TT
tt_bucket_t tt[TT_BUCKETS];		/* Transposition table */
#endif

FILE *in, *out, *err;

/*
	For fast move generation, a look-up table is kept.
	For each piece, on each location, a list of rays along which this piece
	can move is stored. Each ray consists of a number of positions to which a
	piece can move, if it is not blocked on a field earlier in the ray.
	A checked queen can only move to the first field of each ray.
*/
position_t rays[PIECES][FIELDS][MAX_RAYS_PER_PIECE+1][MAX_FIELDS_PER_RAY+1];

/*	The surrounding positions for a given position. Used for generating checked
	queen moves, counting free fields around the queen and checking for a
	connected board of tiles. */
position_t adjacent_fields[FIELDS][9];

/*
	To determine if a piece is in a position to check a queen, a look-up table
	is kept, which maps a piece, the field for this piece and the field for
	the queen, to a ray that needs to be followed to make sure it is a check.
	Many entries contain NULL pointers, which mean that this configuration
	can not cause a check.
	A non-null pointer immediately indicates check in the case of knights.
	This table can also be used to check for queens facing each other.
*/
position_t *checks[PIECES][FIELDS][FIELDS];

/* The book of openings. Defined below. */
#ifdef USE_BOOK
const book_t books[BOOKS];
bookmark_t bookmarks[BOOKS];
#endif

/* Global variable; a bit of a hack, used to return a list of best
	moves from ab_search. */
#ifdef BUILD_BOOK
valued_move_t *cur_vm_list;
int *cur_vm_size;
#endif

#ifndef RETURN_CHECK
/* Returns if the current player is checked, in the situation
	when generate_moves() was last called. */
BOOL checked
#ifdef DEBUG
	= 123
#endif
	;
#endif


#ifdef CONNECTED_CACHE
static struct {
	bitboard_t tiles;
	BOOL       connected;
} connected_cache[CONNECTED_CACHE];
#endif


#ifdef CHECK_CACHE
static struct {
	positions_t current, other;
	BOOL        checked;
} check_cache[CHECK_CACHE];
#endif

/* Timer */
signed long
	time_s, time_ms,
	time_total_ms = 0;
BOOL
	timer_running = FALSE;


/* For debugging purposes: */
#ifdef DEBUG
unsigned long
	total_situations,
	total_moves;

#ifdef TT
unsigned long 
	tt_hits = 0,
	tt_far_misses = 0,
	tt_near_misses = 0;
#endif
#endif


/*
	Prototypes
*/

void initialize();

INLINE unsigned char encode_move(move_t move);
INLINE move_t decode_move(unsigned char code);

INLINE void execute(move_t move);
INLINE void execute_for_check(move_t move);
INLINE void undo();

INLINE BOOL connected(bitboard_t tiles);
INLINE BOOL may_remove_tile(position_t src);
INLINE BOOL may_move_tile(position_t src, position_t dst);

INLINE BOOL current_is_checked();
INLINE BOOL other_is_checked();

INLINE unsigned int generate_moves(move_t *move
#ifdef RETURN_CHECK
                                  , BOOL *checked
#endif
                                  );
INLINE value_t evaluate();

const char *format_move(move_t move);
void fprint_field(FILE *fp);

/*
	Timer functions
*/

void timer_start()
{
#	ifdef WIN32
		struct _timeb tb;
		_ftime(&tb);
		time_s = tb.time;
		time_ms = tb.millitm;
#	else
		struct timeval tv;
		gettimeofday(&tv, NULL);
		time_s = tv.tv_sec;
		time_ms = tv.tv_usec / 1000;
#	endif

	assert(timer_running == FALSE);
	timer_running = TRUE;
}

void timer_stop()
{
#	ifdef WIN32
		struct _timeb tb;
		_ftime(&tb);
		time_total_ms +=
			((tb.time - time_s) * 1000) +
			(tb.millitm - time_ms);
#	else
		struct timeval tv;
		gettimeofday(&tv, NULL);
		time_total_ms +=
			((tv.tv_sec - time_s) * 1000) +
			((tv.tv_usec / 1000) - time_ms);
#	endif

	time_total_ms += 10; /* safety margin */

	assert(timer_running == TRUE);
	timer_running = FALSE;
}

unsigned long timer_query()	/* result is in ms */
{
	if(timer_running)
	{
		/* Add currently measured time to total */
		long
			old_s = time_s,
			old_ms = time_ms;

#		ifdef WIN32
			struct _timeb tb;
			_ftime(&tb);
			time_s = tb.time;
			time_ms = tb.millitm;
#		else
			struct timeval tv;
			gettimeofday(&tv, NULL);
			time_s = tv.tv_sec;
			time_ms = tv.tv_usec / 1000;
#		endif

		time_total_ms += (1000 * (time_s - old_s)) + (time_ms - old_ms);
	}
	
	return time_total_ms;
}

/*
	Game functions
*/

int step_valid(int x, int y, int dx, int dy)
{
	return ((x + dx >= 0) && (y + dy >= 0) && (x + dx < SIDE) && (y + dy < SIDE));
}

int trace_ray(position_t pos, int dx, int dy, position_t *ray)
{
	int
		x = POS_X(pos),
		y = POS_Y(pos),
		fields = 0;

	while(step_valid(x, y, dx, dy))
	{
		x += dx; y += dy;
		ray[fields++] = XY_POS(x, y);
	}
	ray[fields] = POS_NULL; /* Mark end of ray */
	return fields;
}

/* Initializes the global data */
void initialize()
{
	/* Set initial game state */
	game = &history[0];

	{
		piece_t p;

		GAME.current.pos[KNIGHT] =  8;
		GAME.current.pos[QUEEN]  =  9;
		GAME.current.pos[BISHOP] = 10;
		GAME.current.pos[ROOK]   = 16;

		GAME.other.pos[ROOK]   = 32;
		GAME.other.pos[BISHOP] = 38;
		GAME.other.pos[QUEEN]  = 39;
		GAME.other.pos[KNIGHT] = 40;

		GAME.tiles = BITS(FIELDS);

		GAME.pieces = 0;
		for(p = 0; p < PIECES; ++p)
		{
			GAME.pieces |= BIT(GAME.current.pos[p]);
			GAME.pieces |= BIT(GAME.other.pos[p]);
		}

		GAME.previous_from = GAME.previous_to = POS_NULL;
	}

	/* Initialize rays table */
	{
		position_t pos;
		/* BISHOP */

		for(pos = 0; pos < FIELDS; ++pos)
		{
			int ray = 0;
			if(trace_ray(pos, -1, -1, rays[BISHOP][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  1, -1, rays[BISHOP][pos][ray]) > 0) ++ray;
			if(trace_ray(pos, -1,  1, rays[BISHOP][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  1,  1, rays[BISHOP][pos][ray]) > 0) ++ray;
			rays[BISHOP][pos][ray][0] = POS_NULL;
		}
		
		/* ROOK */
		for(pos = 0; pos < FIELDS; ++pos)
		{
			int ray = 0;
			if(trace_ray(pos, -1,  0, rays[ROOK][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  0, -1, rays[ROOK][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  1,  0, rays[ROOK][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  0,  1, rays[ROOK][pos][ray]) > 0) ++ray;
			rays[ROOK][pos][ray][0] = POS_NULL;
		}

		/* KNIGHT */
		for(pos = 0; pos < FIELDS; ++pos)
		{
			const struct { int dx, dy; } steps[8] = {
				{ -1, -2 }, { -2, -1 }, {  1, -2 }, { -2,  1 },
				{ -1,  2 }, {  2, -1 }, {  1,  2 }, {  2,  1 }
			};
				
			int
				x = POS_X(pos),
				y = POS_Y(pos),
				step, ray = 0;

			for(step = 0; step < 8; ++step)
				if(step_valid(x, y, steps[step].dx, steps[step].dy))
				{
					rays[KNIGHT][pos][ray][0] =
						XY_POS(x + steps[step].dx, y + steps[step].dy);
					rays[KNIGHT][pos][ray][1] = POS_NULL; /* mark end of ray */
					++ray;
				}

			rays[KNIGHT][pos][ray][0] = POS_NULL;
		}

		/* QUEEN */
		for(pos = 0; pos < FIELDS; ++pos)
		{
			int ray = 0;
			if(trace_ray(pos, -1, -1, rays[QUEEN][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  1, -1, rays[QUEEN][pos][ray]) > 0) ++ray;
			if(trace_ray(pos, -1,  1, rays[QUEEN][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  1,  1, rays[QUEEN][pos][ray]) > 0) ++ray;
			if(trace_ray(pos, -1,  0, rays[QUEEN][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  0, -1, rays[QUEEN][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  1,  0, rays[QUEEN][pos][ray]) > 0) ++ray;
			if(trace_ray(pos,  0,  1, rays[QUEEN][pos][ray]) > 0) ++ray;
			rays[QUEEN][pos][ray][0] = POS_NULL;
		}
	}

	/* Initialize checks table */
	{
		piece_t piece;
		position_t pos_piece, pos_queen;

		for(piece = 0; piece < PIECES; ++piece)
			for(pos_piece = 0; pos_piece < FIELDS; ++pos_piece)
				for(pos_queen = 0; pos_queen < FIELDS; ++pos_queen)
				{
					int ray, field;
					checks[piece][pos_piece][pos_queen] = NULL;
					for(ray = 0; rays[piece][pos_piece][ray][0] != POS_NULL; ++ray)
						for(field = 0; rays[piece][pos_piece][ray][field] != POS_NULL; ++field)
							if(rays[piece][pos_piece][ray][field] == pos_queen)
								checks[piece][pos_piece][pos_queen] =
									rays[piece][pos_piece][ray];
				}
	}

	/* Initialize adjacent fields table */
	{
		position_t pos;
		for(pos = 0; pos < FIELDS; ++pos)
		{
			int dx, dy, field = 0;
			for(dx = -1; dx <= 1; ++dx)
			for(dy = -1; dy <= 1; ++dy)
				if(	((dx != 0) || (dy != 0)) &&
					 (step_valid(POS_X(pos), POS_Y(pos), dx, dy)))
					adjacent_fields[pos][field++] = XY_POS(POS_X(pos)+dx, POS_Y(pos)+dy);
			adjacent_fields[pos][field] = POS_NULL;
		}
	}

#	ifdef CONNECTED_CACHE
	/* Initialize cache for connected() function */
	{
		int n;
		for (n = 0; n < CONNECTED_CACHE; ++n)
		{
			connected_cache[n].tiles = 0;
			connected_cache[n].connected = FALSE;
		}
	}
#	endif

#	ifdef CHECK_CACHE
	/* Initialize cache for check_for_check() function */
	{
		int n;
		for (n = 0; n < CHECK_CACHE; ++n)
		{
			check_cache[n].current.value = GAME.current.value;
			check_cache[n].other.value   = GAME.other.value;
			check_cache[n].checked       = FALSE;
		}
	}
#	endif

#	ifdef USE_BOOK
	{
		int b;
		long n;
		for(b = 0; b < BOOKS; ++b)
		{
#			ifdef DEBUG
				fprintf(err, "Using book \"%s\" with %i entries.\n", books[b].id, books[b].total_size);
#			endif
			bookmarks[b].entry = books[b].entries;
			bookmarks[b].section_size = books[b].total_size;
			
			for(n = 0; n < books[b].total_size; ++n)
				if(books[b].entries[n] == 254)
				{
#					ifdef DEBUG
						fprintf(err, "Book \"%s\" contains %i empty entries.\n", books[b].id, books[b].total_size - n);
#					endif
					while(n < books[b].total_size)
						books[b].entries[n++] = 255;
				}
		}
	}
#	endif
}

#ifdef TT
void clear_tt()
{
#	ifdef DEBUG
	if(tt_hits + tt_far_misses + tt_near_misses > 0)
	{
		fprintf(err, "\tTransposition table:\n");
		fprintf(err, "\t\t%6i hits\n", tt_hits);
		fprintf(err, "\t\t%6i far misses\n", tt_far_misses);
		fprintf(err, "\t\t%6i near misses\n", tt_near_misses);
		fprintf(err, "\t\tHit ratio: %.5f%%\n",
			(double)tt_hits/(tt_hits+tt_far_misses+tt_near_misses));
		fflush(err);

		tt_hits = 0; tt_far_misses = 0; tt_near_misses = 0;
	}
#	endif

	memset(tt, 0, sizeof(tt));
}
#endif

INLINE BOOL connected(bitboard_t tiles)
{
#	ifdef CONNECTED_CACHE
	int cache_index = (int)(tiles % ((int64)CONNECTED_CACHE));

	{
		if(connected_cache[cache_index].tiles == tiles)
			return connected_cache[cache_index].connected;
	}
#	endif

	{
		position_t q[FIELDS], qi = 0, ql = 0;
		bitboard_t unvisited = tiles;

		/* Find a first tile to start with */
		{
			register position_t p = 0;
			bitboard_t valid = 1; 
			while(!(tiles & valid))
			{
				++p; valid <<= 1;
			}

			assert(POS_IS_VALID(p));

			unvisited ^= BIT(p);
			q[ql++] = p;
		}

		while(qi < ql)
		{
			register position_t p = q[qi++], *rs;

			for(rs = adjacent_fields[p]; (*rs) != POS_NULL; ++rs)
				if(unvisited & BIT(*rs))
				{
					unvisited ^= BIT(*rs);
					q[ql++] = *rs;
				}
		}

#		ifdef CONNECTED_CACHE
			connected_cache[cache_index].tiles = tiles;
			return (( connected_cache[cache_index].connected = ((BOOL)(unvisited == 0)) ));
#		else
			return ((BOOL)(unvisited == 0));
#		endif
	}
}

INLINE BOOL may_remove_tile(position_t src)
{
	return connected(GAME.tiles ^ BIT(src));
}

INLINE BOOL may_move_tile(position_t src, position_t dst)
{
	return connected(GAME.tiles ^ (BIT(src) | BIT(dst)));
}

INLINE BOOL check_for_check(position_t pos_queen, position_t *pos)
{
	register position_t *ray;

#if 0
	piece_t piece;
	position_t field;
	for(piece = 0; piece < PIECES; ++piece)
		if((pos[piece] != POS_NULL) && (ray = checks[piece][pos[piece]][pos_queen]) != NULL)
			for(field = 0; ray[field] != POS_NULL; ++field)
				if(GAME.pieces & BIT(ray[field]))
				{
					if(ray[field] == pos_queen)
						return TRUE;
					break;
				}

#else /* UNROLLED VERSION FOLLOWS */

	if((pos[KNIGHT] != POS_NULL) && (checks[KNIGHT][pos[KNIGHT]][pos_queen]) != NULL)
		return TRUE;	/* knights can never be blocked */

	if((pos[QUEEN] != POS_NULL) && (ray = checks[QUEEN][pos[QUEEN]][pos_queen]) != NULL)
		for( ; *ray != POS_NULL; ++ray)
			if(GAME.pieces & BIT(*ray))
			{
				if(*ray == pos_queen)
					return TRUE;
				break;
			}
	if((pos[ROOK] != POS_NULL) && (ray = checks[ROOK][pos[ROOK]][pos_queen]) != NULL)
		for( ; *ray != POS_NULL; ++ray)
			if(GAME.pieces & BIT(*ray))
			{
				if(*ray == pos_queen)
					return TRUE;
				break;
			}

	if((pos[BISHOP] != POS_NULL) && (ray = checks[BISHOP][pos[BISHOP]][pos_queen]) != NULL)
		for( ; *ray != POS_NULL; ++ray)
			if(GAME.pieces & BIT(*ray))
			{
				if(*ray == pos_queen)
					return TRUE;
				break;
			}
#endif

	return FALSE;
}

INLINE BOOL current_is_checked()
{
#	ifdef CHECK_CACHE
	{
		int cache_index = ((GAME.current.value ^
			(7 * (GAME.other.value))) % CHECK_CACHE);

		if((check_cache[cache_index].current.value == GAME.current.value) &&
		   (check_cache[cache_index].other.value == GAME.other.value))
			return check_cache[cache_index].checked;

		check_cache[cache_index].current.value = GAME.current.value;
		check_cache[cache_index].other.value   = GAME.other.value;
		return ((check_cache[cache_index].checked = 
				check_for_check(GAME.current.pos[QUEEN], GAME.other.pos)
			));
	}
#	else
		return check_for_check(GAME.current.pos[QUEEN], GAME.other.pos);
#	endif
}

INLINE BOOL other_is_checked()
{
#	ifdef CHECK_CACHE
	{
		int cache_index = ((GAME.other.value ^
			(7 * (GAME.current.value))) % CHECK_CACHE);

		if((check_cache[cache_index].current.value == GAME.other.value) &&
		   (check_cache[cache_index].other.value == GAME.current.value))
			return check_cache[cache_index].checked;

		check_cache[cache_index].other.value   = GAME.current.value;
		check_cache[cache_index].current.value = GAME.other.value;
		return ((check_cache[cache_index].checked = 
				check_for_check(GAME.other.pos[QUEEN], GAME.current.pos)
			));
	}
#	else
		return check_for_check(GAME.other.pos[QUEEN], GAME.current.pos);
#	endif

}

/* TODO: optimize this! */
/* Optimaliseren voor queen moves, swap moves, basic moves? */
INLINE void candidate_move(piece_t piece, position_t src, position_t dst, move_t **move_list)
{
	move_t move;
	move.piece = piece;
	move.to = dst;

	/* Verify this move has not been played before */
	if((src == GAME.previous_from) && (dst == GAME.previous_to))
		return;

	/*	Check for tiles/connection rule. The queen-check was already done
		earlier in the process of move generation. */
	if(!(
		  (GAME.tiles & BIT(dst)) ||
		  ((piece != QUEEN) && may_move_tile(src,dst))
	))
		return;


	/* Check for check */
	{
		BOOL checked = 0;
		execute_for_check(move);
		checked = other_is_checked();
		undo();
		if(checked)
		{
#if DEBUG > 20
			printf("((( %s ))) ",format_move(move));
#endif
			return;
		}
	}
	
	(**move_list) = move;
	++(*move_list);
}

INLINE unsigned int generate_moves(move_t *initial_move_list
#ifdef RETURN_CHECK
								   , BOOL *checked
#endif                            
								   )
{
	move_t *move_list = initial_move_list;
	position_t src;
	int ray;
	register position_t dst, *fields;
#ifdef RETURN_CHECK
	if(checked != NULL) (*checked) = current_is_checked();
#else
	checked = current_is_checked();
#endif


#if 0
	piece_t piece;

	/* Generate moves for basic pieces */
	for(piece = 0; piece < QUEEN; ++piece)
	{
		src = GAME.current.pos[piece];
		if(src == POS_NULL)
			continue;	/*	Piece was removed from the board;
							do not generate moves for it. */

		/* Trace all rays */
		for(ray = 0; *(fields = rays[piece][src][ray]) != POS_NULL; ++ray)
			do
			{
				dst = *fields;

				/* Check for collission */
				if(BIT(dst) & GAME.pieces)
				{
					if(GAME.current.pos[QUEEN] == dst)
						break; /* We hit our own queen! */

					/* Try to add this move */
					candidate_move(piece, src, dst, &move_list);

					/*	We hit a piece, so end tracing this ray. */
					break;
				}

				/* Try to add this move */
				candidate_move(piece, src, dst, &move_list);
			}
			while(*(++fields) != POS_NULL);
	}
#else /* UNROLLED VERSION FOLLOWS */

	if((src = GAME.current.pos[BISHOP]) != POS_NULL)
		for(ray = 0; *(fields = rays[BISHOP][src][ray]) != POS_NULL; ++ray)
		do
		{
			dst = *fields;
			if(BIT(dst) & GAME.pieces)
			{
				if(GAME.current.pos[QUEEN] == dst)
					break;
				candidate_move(BISHOP, src, dst, &move_list);
				break;
			}
			candidate_move(BISHOP, src, dst, &move_list);
		}
		while(*(++fields) != POS_NULL);

	if((src = GAME.current.pos[ROOK]) != POS_NULL)
		for(ray = 0; *(fields = rays[ROOK][src][ray]) != POS_NULL; ++ray)
		do
		{
			dst = *fields;
			if(BIT(dst) & GAME.pieces)
			{
				if(GAME.current.pos[QUEEN] == dst)
					break;
				candidate_move(ROOK, src, dst, &move_list);
				break;
			}
			candidate_move(ROOK, src, dst, &move_list);
		}
		while(*(++fields) != POS_NULL);

	if((src = GAME.current.pos[KNIGHT]) != POS_NULL)
		for(ray = 0; (dst = rays[KNIGHT][src][ray][0]) != POS_NULL; ++ray)
		{
			if(GAME.current.pos[QUEEN] == dst)
					continue;
			candidate_move(KNIGHT, src, dst, &move_list);
		}
#endif

	/* Generate queen moves */
	src = GAME.current.pos[QUEEN];
	if(may_remove_tile(src))
	{
#ifdef RETURN_CHECK
		BOOL is_checked = (checked == NULL) ? current_is_checked() : (*checked);
		if(is_checked)
#else
		if(checked)			
#endif
		{
			position_t *fields;
			for(fields = adjacent_fields[src]; (dst = *fields) != POS_NULL; ++fields)
			{
				/* Check for collission */
				if(BIT(dst) & GAME.pieces)
					if ((GAME.current.pos[ROOK]   == dst) ||
						(GAME.current.pos[KNIGHT] == dst) ||
						(GAME.current.pos[BISHOP] == dst))
							continue; /* The queen hit one of her own pieces! */

				/* Try to add this move */
				candidate_move(QUEEN, src, dst, &move_list);
			}

		} else
		{
			/* Trace all rays */
			for(ray = 0; *(fields = rays[QUEEN][src][ray]) != POS_NULL; ++ray)
				do
				{
					dst = *fields;

					/* Check for collission */
					if(BIT(dst) & GAME.pieces)
					{
						if ((GAME.current.pos[ROOK]   == dst) ||
							(GAME.current.pos[KNIGHT] == dst) ||
							(GAME.current.pos[BISHOP] == dst))
								break; /* The queen hit one of her own pieces! */

						/* Try to add this move */
						candidate_move(QUEEN, src, dst, &move_list);

						/*	We captured a piece, so end tracing this ray. */
						break;
					}

					/* Try to add this move */
					candidate_move(QUEEN, src, dst, &move_list);
				}
				while(*(++fields) != POS_NULL);
		}
	}

	return (move_list - initial_move_list);
}

/* Executes a move */
INLINE void execute(move_t move)
{
	game_t *next_game = &(game[1]);
	register piece_t piece;

	bitboard_t src_bit = BIT(next_game->previous_from = game->current.pos[move.piece]);
	bitboard_t dst_bit = BIT(next_game->previous_to = move.to);

	assert(next_game <= &(history[HISTORY_LENGTH-1]));

#	ifdef DEBUG
		++total_moves;
#	endif

#	ifndef RETURN_CHECK
		/* Invalidate 'checked'! */
		checked = 123;
#	endif

	next_game->current.value = game->other.value;
	next_game->other.value = game->current.value;
	next_game->other.pos[move.piece] = move.to;

	if(move.piece == QUEEN)
	{
		/* Clear tile bit on source field */
		next_game->tiles = game->tiles ^ src_bit; 

		/* Clear piece bit on source field and set piece bit on destination field */
		next_game->pieces = (game->pieces ^ src_bit) | dst_bit;

		/* Capture piece, if it's there */
		if(game->pieces & dst_bit)
		{
			/*	FIXME: unroll manually; the compiler won't do it
				in the CodeCup configuration */
			for(piece = 0; piece < QUEEN; ++piece)
				if(game->other.pos[piece] == move.to)
				{
					next_game->current.pos[piece] = POS_NULL;
					break;
				}
			assert(PIECE_IS_VALID(piece) && (piece != QUEEN));
		}
	}
	else /* basic piece */
	{
		/* Switch tile bits, if the destination field does not have a tile */
		next_game->tiles = (game->tiles & dst_bit) ? game->tiles :
			game->tiles ^ (dst_bit | src_bit);

		/* Switch piece bits, if the destination field does not have a piece */
		if(!(game->pieces & dst_bit))
		{
			next_game->pieces = game->pieces ^ (dst_bit | src_bit);
		}
		else
		{
			next_game->pieces = game->pieces;

			/* Update the piece that is switched away as well */
			/* FIXME: THIS IS COSTLY! :-( */
			for(piece = 0; piece < PIECES; ++piece)
				if(game->current.pos[piece] == move.to)
				{
					next_game->other.pos[piece] = game->current.pos[move.piece];
					break;
				}
				else
				if(game->other.pos[piece] == move.to)
				{
					next_game->current.pos[piece] = game->current.pos[move.piece];
					break;
				}
			assert(PIECE_IS_VALID(piece));
		}
	}

	game = next_game;
}

/* Executes a move, so that check_for_check can run. This does a bit less than execute()! */
INLINE void execute_for_check(move_t move)
{
	game_t *next_game = &(game[1]);
	register piece_t piece;

	/* Don't update previous_from/to fields */
	bitboard_t src_bit = BIT(game->current.pos[move.piece]);
	bitboard_t dst_bit = BIT(move.to);

	assert(next_game <= &(history[HISTORY_LENGTH-1]));

	next_game->current.value = game->other.value;
	next_game->other.value = game->current.value;
	next_game->other.pos[move.piece] = move.to;

	if(move.piece == QUEEN)
	{
		/* Clear tile bit on source field */
		/* -- Tiles are not relevant when checking for check */

		/* Clear piece bit on source field and set piece bit on destination field */
		next_game->pieces = (game->pieces ^ src_bit) | dst_bit;

		/* Capture piece, if it's there */
		/* -- No need to: it can never check the queen if it's on the same square! */
	}
	else /* basic piece */
	{
		/* Switch tile bits, if the destination field does not have a tile */
		/* -- Tiles are not relevant when checking for check */

		/* Switch piece bits, if the destination field does not have a piece */
		if(!(game->pieces & dst_bit))
		{
			next_game->pieces = game->pieces ^ (dst_bit | src_bit);
		}
		else
		{
			next_game->pieces = game->pieces;

			/* Update the piece that is switched away as well */
			/* FIXME: THIS IS COSTLY! :-( */
			for(piece = 0; piece < PIECES; ++piece)
				if(game->current.pos[piece] == move.to)
				{
					next_game->other.pos[piece] = game->current.pos[move.piece];
					break;
				}
				else
				if(game->other.pos[piece] == move.to)
				{
					next_game->current.pos[piece] = game->current.pos[move.piece];
					break;
				}
			assert(PIECE_IS_VALID(piece));
		}
	}

	game = next_game;
}

/* Takes back a move */
INLINE void undo()
{
	--game;
	assert(game >= &(history[0]));
}

/*
	Input/output and main functions.
*/
move_t parse_move(const char *buf)
{
	assert(buf[0] >= 'a' && buf[0] <= 'g');
	assert(buf[1] >= '1' && buf[1] <= '7');
	assert(buf[2] == '-');
	assert(buf[3] >= 'a' && buf[3] <= 'g');
	assert(buf[4] >= '1' && buf[4] <= '7');
	assert(buf[5] == '\0');

	{
		move_t result;
		piece_t piece;
		position_t
			from = XY_POS(buf[0]-'a', buf[1]-'1');

		assert(POS_IS_VALID(from));

		/* Find the piece */
		for(piece = 0; piece < PIECES; ++piece)
			if((GAME.current.pos[piece] == from) || (GAME.other.pos[piece] == from))
				break;

		result.piece = piece;
		assert(PIECE_IS_VALID(piece));
		result.to = XY_POS(buf[3]-'a', buf[4]-'1');
		assert(POS_IS_VALID(result.to));

		assert(MOVE_IS_VALID(result));
		return result;
	}
}

const char *format_move(move_t move)
{
	static char buf[6];
	position_t from = GAME.current.pos[move.piece];
	assert(MOVE_IS_VALID(move));
	sprintf(buf, "%c%c-%c%c",
		POS_X(from)+'a', POS_Y(from)+'1', 
		POS_X(move.to)+'a', POS_Y(move.to)+'1'
	);
	return buf;
}

void fprint_field(FILE *fp)
{
	int x, y;

	for(y = SIDE - 1; y>=0; --y)
	{
		fprintf(fp,"%c", '1'+y);
		for(x = 0; x < SIDE; ++x)
		{
			char chr;
			position_t pos = XY_POS(x,y);

			if(!(GAME.tiles & BIT(pos)))
				chr = '*';
			else
			if(GAME.pieces & BIT(pos))
			{
				piece_t p;
				colour_t c = WHITE; /* avoid warning */

				for(p = 0; p < PIECES; ++p)
					if(GAME.current.pos[p] == pos)
					{
						c = WHITE;
						break;
					} else
					if(GAME.other.pos[p] == pos)
					{
						c = BLACK;
						break;
					}

				if(c == COLOURS)
					chr = '?';
				else
				{
					const char piece_chr[COLOURS][PIECES] = {
						{ 'B', 'R', 'K', 'Q'},
						{ 'b', 'r', 'k', 'q'}
					};
					if(CURRENT_PLAYER == BLACK)
						c = OTHER_COLOUR(c);
					chr = piece_chr[c][p];
				}
			}
			else
				chr = '.';

			fprintf(fp, "%c", chr);
			++pos;
		}
		fprintf(fp,"\n");
	}
	fprintf(fp," ");
	for(x = 0; x < SIDE; ++x)
		fprintf(fp, "%c", 'A'+x);
	fprintf(fp,"  %s to move\n", CURRENT_PLAYER == BLACK ? "Black" : "White");
}

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

int compare_valued_moves(const void *a, const void *b)
{
	return ((((valued_move_t*)b)->value) - (((valued_move_t*)a)->value));
}

#ifdef QSEARCH /* Quiesence search */

#ifdef DEBUG
int qstat = 0;
#endif

int qs_disable = 0;

value_t q_search(int depth, int max_depth, value_t alpha, value_t beta)
{
	value_t v;
	
	v = evaluate(depth);	/* updates 'checked' to reflect te current situation */

#ifdef DEBUG
	++qstat;
#endif

	if(depth == max_depth)
		return v;

	if(v >= beta)
		return beta;

	if(v > alpha)
		alpha = v;

	{
		move_t move_list[MAX_MOVES];
		BOOL is_checked;
		int m, moves = generate_moves(move_list
#ifdef RETURN_CHECK
			, &is_checked
#endif
			);

#		ifndef RETURN_CHECK
			is_checked = checked;
#		endif

		for(m = 0; m < moves; ++m)
		{
			BOOL tactical =
				is_checked ||
				((move_list[m].piece == QUEEN) && (GAME.pieces & BIT(move_list[m].to)));

			assert(is_checked != 123);

			execute(move_list[m]);

			tactical = tactical || current_is_checked();

			if(tactical)
				v = -q_search(depth + 1, max_depth, -beta, -alpha);

			undo();

			if(!tactical)
				continue;

			if(v >= beta)
				return beta;

			if(v > alpha)
				alpha = v;
		}

	}

	return alpha;
}

#endif //def QSEARCH

value_t ab_search(int depth, int max_depth, value_t alpha, value_t beta, move_t *result)
{
#ifdef TT
	int tt_index;

	if(depth != max_depth)
	{
		/* Attempt to locate this position in the transposition table */
		tt_index = HASH_GAME(GAME, TT_BUCKETS);

		if(tt[tt_index].depth >= max_depth - depth)
		{
			/* Bucket is valid; verify position */
			if(
				(tt[tt_index].tiles == GAME.tiles) &&
				(tt[tt_index].current.value == GAME.current.value) &&
				(tt[tt_index].other.value == GAME.other.value)
			)
			{
#				ifdef DEBUG
					++tt_hits;
#				endif

				/* Current position was found! */
				if(
					(tt[tt_index].previous_from == GAME.previous_from) &&
					(tt[tt_index].previous_to == GAME.previous_to)
				)
				{
					if(result != NULL)
						(*result) = tt[tt_index].best_move;
					return tt[tt_index].value;
				}
				else
					if(
						(GAME.current.pos[tt[tt_index].best_move.piece] !=
						 GAME.previous_from) ||
						(tt[tt_index].best_move.to != GAME.previous_to)
					)
					{
						move_t move;
						value_t v;

						move.to = tt[tt_index].previous_to;
						for(move.piece = 0; move.piece < PIECES; ++move.piece)
							if(GAME.current.pos[move.piece] == tt[tt_index].previous_from)
								break;

						/* If piece invalid, previous last move is not possible now.
							Can this be??? NO! FIX THIS! */
						if(PIECE_IS_VALID(move.piece))
						{
							alpha = tt[tt_index].value;
							execute(move);
							v = -ab_search(depth + 1, max_depth, -beta, -alpha, NULL);
							undo();

							if(v > alpha)
							{
								if(result != NULL)
									(*result) = move;
								return v;
							}
						}

						if(result != NULL)
							(*result) = tt[tt_index].best_move;
						return alpha;
					}
						
			}
#			ifdef DEBUG
			else
				++tt_near_misses;
#			endif
		}
#		ifdef DEBUG
		else
			++tt_far_misses;
#		endif
	}
#endif

	if(depth == max_depth)
	{
#		ifdef QSEARCH
			return qs_disable ? evaluate(depth) : 
				q_search(depth, depth + QSEARCH_DEPTH, alpha, beta);
#		else
			return evaluate(depth);
#		endif
	}

	{
		move_t move_list[MAX_MOVES];
		int m, best_m = -1, moves;
		value_t v = VAL_WORST;
#ifdef RETURN_CHECK
		BOOL checked;
#endif

#		ifdef PV
			BOOL pv = FALSE;
#		endif

		moves = generate_moves(move_list
#ifdef RETURN_CHECK
			, &checked
#endif
			);

		assert(checked != 123);

		/* Check for end of game */
		if(moves == 0)
			return (checked ? depth - VAL_CHECKMATE: 0);

#		ifdef DEBUG
			total_moves += moves;
#		endif


#		ifdef PREORDER_MOVES
#			ifdef QSEARCH
				++qs_disable;
#			endif
			if((max_depth + PREORDER_DEPTH >= depth) && (moves > 1))
			{
				valued_move_t vm_list[MAX_MOVES];

				for(m = 0; m < moves; ++m)
				{
					execute((vm_list[m].move = move_list[m]));
					vm_list[m].value =
						-ab_search(depth + 1, max_depth + 1 + PREORDER_DEPTH,
							-beta, -alpha, NULL);
					undo();
				}

				qsort((void*)&vm_list[0], moves, sizeof(vm_list[0]), compare_valued_moves);

				for(m = 0; m < moves; ++m)
					move_list[m] = vm_list[m].move;
			}
#			ifdef QSEARCH
				--qs_disable;
#			endif
#		endif

		for(m = 0; m < moves; ++m)
		{
#			ifdef DEBUG
				if((depth == 0) && (err == stderr))
					fprintf(err,"\r%3i%%", (100*m)/moves);
#			endif

			execute(move_list[m]);
#			ifdef PV
				if(pv)
				{
					v = -ab_search(depth + 1, max_depth, -(alpha+1), -alpha, NULL);
					if((v > alpha) && (v < beta))	/* assumption failed */
						v = -ab_search(depth + 1, max_depth, -beta, -alpha, NULL);
				} else
					v = -ab_search(depth + 1, max_depth, -beta, -alpha, NULL);
#			else
				v = -ab_search(depth + 1, max_depth, -beta, -alpha, NULL);
#			endif
			undo();

#			ifdef BUILD_BOOK
			if(depth == 0)
			{
				cur_vm_list[m].move = move_list[m];
				cur_vm_list[m].value = v; /* negative, so they are sorted best-first */
			}
#			endif

#			ifdef BUILD_BOOK
			if(depth > 0)
			{
#			endif

			if(v >= beta)	/* can not happen at depth == 0 */
				return beta;

			if(v > alpha)
			{
				best_m = m;
				alpha = v;
#				ifdef PV
					pv = TRUE;
#				endif
			}

#			ifdef BUILD_BOOK
			}
#			endif
		}

#		ifdef DEBUG
			if((depth == 0) && (err == stderr))
				fprintf(err,"\r100%%\n");
#		endif

#		ifdef BUILD_BOOK
		if(depth == 0)
		{
			*cur_vm_size = moves;
			qsort((void*)cur_vm_list, moves, sizeof(cur_vm_list[0]), compare_valued_moves);
		}
		#endif

#		ifdef TT
			if((max_depth - depth >= TT_MIN_DEPTH) && (best_m >= 0))
			{
				assert((best_m >= 0) && (best_m < moves));

				/* Store this position in the transposition table */
				tt[tt_index].tiles = GAME.tiles;
				tt[tt_index].current.value = GAME.current.value;
				tt[tt_index].other.value = GAME.other.value;
				tt[tt_index].value = alpha;
				tt[tt_index].depth = (unsigned short)(max_depth - depth);
				tt[tt_index].previous_from = GAME.previous_from;
				tt[tt_index].previous_to = GAME.previous_to;
				tt[tt_index].best_move = move_list[best_m];
			}
#		endif

		if((result != NULL) && (best_m >= 0))	/* if best_m >= 0, ab-window failed */
		{
			assert((best_m >= 0) && (best_m < moves));
			*result = move_list[best_m];
		}
		return alpha;
	}
}

move_t select_random_move()
{
	move_t
		move = { PIECE_NULL, POS_NULL },
		move_list[MAX_MOVES];
	int moves;

	moves = generate_moves(move_list
#ifdef RETURN_CHECK
			, NULL
#endif
		);

	if(moves > 0)
		move = move_list[rand()%moves];

	return move;
}

move_t search_for_move()
{
	value_t v;
	move_t best_move = { PIECE_NULL, POS_NULL };
	int depth;
	long ms_start = timer_query();

	move_t move_list[MAX_MOVES];
	int moves;

#	ifdef DEBUG
	long duration, previous_duration = 0;
#	endif

	moves = generate_moves(move_list
#ifdef RETURN_CHECK
			, NULL
#endif
		);
	assert(moves > 0);

	if(moves == 1)
	{
#		ifdef DEBUG
			fprintf(stderr, "A single move available!\n");
#		endif
		return move_list[0];
	}

	for(depth = MIN_SEARCH_DEPTH; depth <= MAX_SEARCH_DEPTH; depth += SEARCH_DEPTH_INC)
	{
		move_t new_move;
#ifdef ASPIRATION_WINDOW
		static value_t
			alpha = -ASPIRATION_WINDOW,
			beta  = +ASPIRATION_WINDOW;
#ifdef DEBUG
		static
			v_old = 0;
#endif
#endif

#		ifdef DEBUG
		duration = (long)timer_query();
#		endif

#		ifdef TT
		clear_tt();
#		endif

#		ifdef ASPIRATION_WINDOW
			v = ab_search(0, depth, alpha, beta, &new_move);
			if((v <= alpha) || (v >= beta))
				v = ab_search(0, depth, VAL_WORST, -VAL_WORST, &new_move);
			alpha = v - ASPIRATION_WINDOW;
			beta  = v + ASPIRATION_WINDOW;

#			ifdef	DEBUG
				fprintf(err, "Difference with previous value: %i\n", v - v_old);
				v_old = v;
#			endif
#else
			v = ab_search(0, depth, VAL_WORST, -VAL_WORST, &new_move);
#endif

		if (v >= -(VAL_CHECKMATE - depth))
			best_move = new_move;

#		ifdef DEBUG
		duration = ((long)timer_query()) - duration;
#		endif

#		ifdef DEBUG
		/* Make debug results so far visible */
		fflush(err);
#		endif

		/* Check mate is good enough! */
		if(v >= (VAL_CHECKMATE - depth))
			break;
	
		{
			long
				moves_left = (MOVES_PLAYED + EXPECTED_MOVES_LEFT) > MAX_TOTAL_MOVES ? MAX_TOTAL_MOVES :
					(MOVES_PLAYED + EXPECTED_MOVES_LEFT),
				deadline = (TOTAL_TIME / moves_left) * (MOVES_PLAYED),
				ms = timer_query();

#			ifdef DEBUG
				fprintf(err, "SCHEDULER: Current time: %li ms\n", ms);
				fprintf(err, "SCHEDULER: Deadline for this move: %li ms\n", deadline);
				fprintf(err, "SCHEDULER: Expected completion time for next depth: %li ms\n",
					ms + ((ms - ms_start) * EXPECTED_BRANCHING_FACTOR));
				if(ms > deadline)
					fprintf(err, "SCHEDULER: WARNING! Deadline exceeded by %li ms!\n", ms - deadline);
				if(previous_duration != 0)
				{
					fprintf(err, "SCHEDULER: This search took %.3f times as long "\
						"as previous one.\n", (double)(duration / previous_duration));
				}
				previous_duration = duration;
#			endif

			if(ms + ((ms - ms_start) * EXPECTED_BRANCHING_FACTOR) > deadline)
				break;
#ifdef DEBUG
			else
				fprintf(err, "SCHEDULER: Authorizing search with depth %i!\n", depth + SEARCH_DEPTH_INC);
#endif

		}
	}

	if(!MOVE_IS_VALID(best_move))
	{
#		ifdef DEBUG
			fprintf(stderr, "WARNING! Selecting random move.\n");
#		endif
		best_move = move_list[rand()%moves];
	}

#	ifdef DEBUG
		if(depth > MAX_SEARCH_DEPTH) depth = MAX_SEARCH_DEPTH;
		if(MOVE_IS_VALID(best_move))
			fprintf(err,"Best move: %s\n", format_move(best_move));
		if(ABS(v) >= VAL_CHECKMATE - depth)
			fprintf(err,"Checkmate found in %i plies! [%i]\n", VAL_CHECKMATE - ABS(v),v);
		else
			fprintf(err,"Value: %i [%i plies]\n", v, depth);
#	endif

	return best_move;
}

/* Encodes a move into a byte, for use with the opening book database. */
INLINE unsigned char encode_move(move_t move)
{
	return (FIELDS * move.piece) + move.to;
}

/* Decodes a byte into a move, for use with the opening book database. */
INLINE move_t decode_move(unsigned char code)
{
	move_t result;
	result.piece = code / FIELDS;
	result.to    = code % FIELDS;
	return result;
}

move_t select_move()
{
#	ifdef USE_BOOK
	{
		int b;
		for(b = 0; b < BOOKS; ++b)
		{
			if(bookmarks[b].entry != NULL)
			{
#				ifdef DEBUG
					fprintf(err, "Move selected from opening book \"%s\".\n", books[b].id);
#				endif
				return decode_move(*(bookmarks[b].entry));
			}
		}
	}
#	endif


	return search_for_move();
}

#ifdef BUILD_BOOK
long skip_book(int depth, long index)
{
	if(depth == BOOK_DEPTH)
		return index;

	{
		int m;
		for(m = 0; m < BOOK_BRANCHES; ++m)
		{
			fprintf(out,"255,"); ++index;
			if((index%20)==0)
				fprintf(out, "\n");
			index = skip_book(depth + 1, index);
		}
	}
	return index;
}

#ifdef BUILD_BOOK
/* Quick 'n' dirty globals for building books */
	time_t time_start;
	long book_size;
#endif

long build_book(int depth, long index)
{
	if(depth == BOOK_DEPTH)
		return index;

	{
		int m;
		valued_move_t vm_list[MAX_MOVES];
		int vm_size = 0;
		int max_depth = (BOOK_DEPTH - (depth - (depth % 2))) + MIN_SEARCH_DEPTH;

		cur_vm_list = vm_list;
		cur_vm_size = &vm_size;
		ab_search(0, (BOOK_SEARCH_DEPTH > max_depth) ? max_depth : BOOK_SEARCH_DEPTH,
			VAL_WORST, -VAL_WORST, NULL);

		if(err == stderr)
		{
			double completed = (double)(index+1)/book_size;
			fprintf(err,"\r%3.4f%% (%3.4f hours remaining)",
				completed * 100.0,
				((double)((time(NULL) - time_start) / 3600.0) / (completed)) -
				((double)((time(NULL) - time_start) / 3600.0))
			);
		}

		if(depth == 0)
		{
			for(m = 0; ((m < vm_size) && (m < BOOK_BRANCHES)); ++m)
				fprintf(err, "\t%s (%i)\n",
					format_move(vm_list[m].move), vm_list[m].value);
		}

		for(m = 0; m < BOOK_BRANCHES; ++m)
		{
			if(m < vm_size)
			{
				fprintf(out, "%3i,", (int)encode_move(vm_list[m].move));
				++index;
				if((index%20)==0)
					fprintf(out, "\n");
				fflush(out);

				execute(vm_list[m].move);
				index = build_book(depth + 1, index);
				undo();
			} else
			{
				fprintf(out, "255,");
				++index;
				if((index%20)==0)
					fprintf(out, "\n");
				index = skip_book(depth + 1, index);
				fflush(out);
			}

		}
	}

	return index;
}
#endif

#ifdef USE_BOOK
unsigned long fprint_book(FILE *fp, const book_t *book, int depth, unsigned long index)
{
	if(depth == book->depth)
		return index;

	{
		int m;
		for(m = 0; m < book->branches; ++m)
		{
			int d;
			fprintf(fp, "%6li: ", index);
			for(d = 0; d < depth; ++d)
				fprintf(fp, "\t");

			if(book->entries[index] == 255)
			{
				fprintf(fp, "-----\n");
				index = fprint_book(fp, book, depth + 1, index + 1);
			} else
			if(book->entries[index] >= 196)
			{
				fprintf(fp, "XXXXX\n");
				index = fprint_book(fp, book, depth + 1, index + 1);
			} else
			{				
				fprintf(fp, "%s\n", format_move(decode_move(book->entries[index])));
				execute(decode_move(book->entries[index]));
				index = fprint_book(fp, book, depth + 1, index + 1);
				undo();
			}
		}
	}

	return index;
}

void follow_bookmark(const book_t *book, bookmark_t *bookmark, move_t move)
{
	if(bookmark->entry == NULL)
		return;

	{
		int n;
		unsigned move_code = encode_move(move);

		for(n = 0; n < book->branches; ++n)
		{
			if(move_code == *(bookmark->entry))
			{
				bookmark->entry = &(bookmark->entry[1]);
				bookmark->section_size = (bookmark->section_size / book->branches) - 1;
				if(bookmark->section_size == 0)
				{
					bookmark->entry = NULL;
#					ifdef DEBUG
						fprintf(err, "End of opening book \"%s\" reached!\n", book->id);
#					endif
				}
				break;
			}
			bookmark->entry = &((bookmark->entry)[bookmark->section_size / book->branches]);
		}

		if(n == book->branches)
		{
#			ifdef DEBUG
				fprintf(err, "Move is not in opening book \"%s\"!\n", book->id);
#			endif
			bookmark->entry = NULL;
		}
	}
}

#endif

void global_execute(move_t move)
{
#	ifdef USE_BOOK
	{
		int b;
		for(b = 0; b < BOOKS; ++b)
			follow_bookmark(&(books[b]), &(bookmarks[b]), move);
	}
#	endif

	execute(move);

#	ifdef DEBUG
		fprint_field(err);
		fprintf(err, "\n");
#	endif

}

int main()
{
	/* Compiler/platform sanity check */
	assert(sizeof(bitboard_t) >= (FIELDS + 7) / 8);
	assert(sizeof(GAME.current) == sizeof(unsigned int));
#ifdef TT
	assert(sizeof(tt) < (1024 * 1024 * 4));
#endif

#	ifdef ERROR
	err=fopen(ERROR, "wt");
	assert(err != 0);
#	else
	err=stderr;
#	endif

#	ifdef OUTPUT
	out=fopen(OUTPUT, "wt");
	assert(out != 0);
#	else
	out=stdout;
#	endif

#	ifdef DEBUG
	fprintf(err, "Debug mode ON.\n");
#	endif

	/* Initialize global data */
	timer_start();	/* START global timer */
	srand(time(NULL));
	initialize();

#	ifdef DEBUG
	fprintf(err, "Initialisation took: %li ms\n\n", timer_query());
	fprint_field(err);
	fprintf(err, "\n");
#	endif

#	ifdef BUILD_BOOK
		fprintf(out, "/* BOOK: %i plies\n\tdepth: %i\n\tbranches: %i\n\n\tHistory:\n",
			BOOK_SEARCH_DEPTH, BOOK_DEPTH, BOOK_BRANCHES);
#	endif
#	ifdef INPUT
	/* Read transcript */
	{
		in=fopen(INPUT, "rt");
		assert(in != 0);

		while(!feof(in))
		{
			char buffer[6];
			move_t move = { POS_NULL };

			/* Lees een zet in */
			if(fscanf(in, "%5s", buffer) != EOF)
			{
				move = parse_move(buffer);

#				ifdef DEBUG
				/* Check that the parsed move is indeed valid */
					{
						int m, moves;
						move_t move_list[MAX_MOVES];
						moves = generate_moves(move_list
						#ifdef RETURN_CHECK
							, NULL
						#endif
						);

						fprintf(err,"Available moves: (%i)\n", moves);
						for(m = 0; m < moves; ++m)
							fprintf(err, "%s\t", format_move(move_list[m]));
						fprintf(err, "\n");

						for(m = 0; m < moves; ++m)
							if(MOVE_EQUALS(move, move_list[m]))
								break;
						assert(m != moves);

					}
					fprintf(err, "Move read: %s\n", format_move(move));
#				endif

#				ifdef BUILD_BOOK
					fprintf(out, "\t%s (code: %i)\n", format_move(move), encode_move(move));
#				endif

				global_execute(move);

			}
		}
		
#		ifdef DEBUG
			fprintf(err, "\nTranscript processed!\n\n");
#		endif

	}
#	endif
#	ifdef BUILD_BOOK
		fprintf(out, "\n*/\n\n");
#	endif

	in=stdin;

#	ifdef PRINT_BOOK
	{
		int b;
		fprintf(err, "Writing out books...\n");
		for(b = 0; b < BOOKS; ++b)
		{
			char filename[128];
			FILE *fp;
			sprintf(filename, "%.100s.txt", books[b].id);
			fp = fopen(filename, "wt");
			assert(fp != NULL);
			fprint_book(fp, &books[b], 0, 0);
			fclose(fp);
		}
		exit(0);
	}
#	endif


#	ifdef ALGORITHM
	for( ; ; )
	{
		char buffer[6];
#		ifdef DEBUG
		int m, moves;
		move_t move_list[MAX_MOVES];
		moves = generate_moves(move_list
		#ifdef RETURN_CHECK
			, NULL
		#endif
		);

		fprintf(err,"Available moves: (%i)\n", moves);
		for(m = 0; m < moves; ++m)
			fprintf(err, "%s\t", format_move(move_list[m]));
		fprintf(err, "\n");

		if(moves == 0)
		{
			fprintf(err, "Opponent has no more moves! Game ends.\n");
			break;
		}
		fflush(err);

#		endif

		timer_stop();	/* SUSPEND global timer! */
		if(fscanf(in, "%5s", buffer) == EOF)
		{
#			ifdef DEBUG
				fprintf(err, "Input stream is empty! Game ends.\n");
#			endif
			break;
		}
		timer_start();	/* RESUME global timer! */

		/* Read other's move */
		if(strcmp(buffer, "start") != 0)
		{
			move_t move = { PIECE_NULL, POS_NULL };
			move = parse_move(buffer);

#			ifdef DEBUG
			/* Check that the parsed move is indeed valid */
			{
				for(m = 0; m < moves; ++m)
					if(MOVE_EQUALS(move, move_list[m]))
						break;
				assert(m != moves);
			}
			fprintf(err, "Move read: %s\n", format_move(move));
#			endif

			global_execute(move);

		}

		/* Generate own move */
		{
			move_t move;

#			if defined(ALGORITHM_RANDOM)
				move = select_random_move();
#			elif defined(ALGORITHM_SEARCH)
				move = select_move();
#			else
#				error("Unsupported algorithm specified!");
#			endif

			if(!MOVE_IS_VALID(move))
			{
#				ifdef DEBUG
					fprintf(err, "No valid move could be selected! Game ends.\n");
#				endif
				break;
			}

			fprintf(out, "%s\n", format_move(move));
			fflush(out);

#			ifdef DEBUG
				fprintf(err, "Move written: %s\n", format_move(move));
#				ifdef QSEARCH
					fprintf(err,"QSTAT == %i\n", qstat);
					qstat = 0;
#				endif
#			endif

			global_execute(move);
		}
	}

#	elif defined(SEARCH_FOR_MATE)

	/* Search for mate. */
	{
		int depth;
		for(depth = 0; depth < 16; ++depth)
		{
			move_t best_move;
			int v = VAL_WORST;
			
#			ifdef DEBUG
			clock_t clock_start, clock_end;

			{
				clock_start = clock();
				total_moves = total_situations = 0;
			}
#			endif

			best_move.piece = PIECE_NULL;

			fprintf(err,"Searching with a depth of %i plies.\n", depth);

#			ifdef TT
			clear_tt();
#			endif
			v = ab_search(0, depth, VAL_WORST, -VAL_WORST, &best_move);

#			if DEBUG >= 25
			{
				int w;
				move_t m;
				w = search(0, depth, &m);
				
				assert(v==w);
			}
#			endif

#			ifdef DEBUG
			clock_end = clock();
			fprintf(err,"\t%li ms taken\n", (1000*(clock_end - clock_start))/CLOCKS_PER_SEC);
			fprintf(err,"\t%li moves executed and undone (%i moves per second)\n", total_moves,
				clock_end - clock_start == 0 ? 0 : (1000 * total_moves) / (clock_end - clock_start));
			fprintf(err,"\t%li situations analysed (%i situations per second)\n", total_situations,
				clock_end - clock_start == 0 ? 0 : (1000 * total_situations) / (clock_end - clock_start));
#			endif

			if(MOVE_IS_VALID(best_move))
				fprintf(err,"Best move: %s\n", format_move(best_move));
			if(ABS(v) >= VAL_CHECKMATE - depth)
			{
				fprintf(err,"Checkmate found! (%i plies)\n", depth);
				break;
			}
			else
				fprintf(err,"Value: %i\n", v);
		}
	}

#	elif defined(BUILD_BOOK)
	{
		long breadth = 1, depth, result;
		book_size = 0;
		for(depth = BOOK_HISTORY_DEPTH; depth < BOOK_DEPTH; ++depth)
		{
			breadth *= BOOK_BRANCHES;
			book_size += breadth;
		}

		fprintf(err, "Opening book size: %lu entries.\n", book_size);

		time_start = time(NULL);
		result = build_book(BOOK_HISTORY_DEPTH, 0);

		fprintf(err, "Opening book written with %lu entries!\n", result);

		assert(result == book_size);
	}


#	endif

#	ifdef DEBUG
		fprintf(err, "Total time passed: %li ms\n", timer_query());
#	endif

	return 0;
}

#undef DEBUG




/* Book of openings follows */

#ifdef USE_BOOK

unsigned char entries_4x12_8plies[22620] = {
 86, /* c3-c6 */
 79,121,158,123,174, 80,129,143,125,194, 30, 78, 86, 81, 79,158,174, 87,123,125,
129,143, 26, 20, 44, 85, 84, 16,129,178,125,123, 83, 30, 80, 77,174, 72,192, 81,
164, 80,123,129,154, 81,125, 72, 77, 51, 83,194,143,150, 83,123,125, 81, 82,129,
143,192, 78, 77, 65, 30,148,178,123,158,129,143,192,125, 65, 81, 83, 80, 86,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, 80,115,180,
123,125, 79,143,158, 81,172,174, 78, 83, 77, 22,123,174,125, 87, 73, 79,143,165,
179, 77, 83, 22, 72, 73,125, 79, 87, 30, 77,129, 44, 26, 20,158, 14, 65,123, 73,
129, 30,125, 79, 87, 81, 83, 82, 77, 78, 26,174, 79, 87, 73, 26,123, 81,143,125,
194,193,151,119,123,174,125,143, 81,158, 82,194, 78, 77,179, 83, 93, 79,143, 87,
123, 73, 30,158, 26, 77,125,129, 20, 16,129,123,125, 83, 87, 77, 73, 81,143,174,
 79, 82, 28,123,129,125,174, 73, 87, 81, 79, 83, 82, 78,143, 34,129, 87,123,125,
174, 73, 77, 81,151,143, 82, 79,  2,123,125,129, 77, 73, 87, 81, 82, 83, 79, 78,
143,121,158,125, 73,123,129,143,174, 87, 79, 78, 81, 83,125,115,180, 80,138, 78,
 83, 77, 79,116,110, 82, 60, 67,121,116, 80,187, 60, 74, 79, 78, 82, 53, 77, 67,
 83, 72,116, 79, 67, 80, 53, 60, 74, 83, 77, 78,130, 82, 65,116, 30,130, 74, 67,
 46, 32, 77, 80, 78, 79, 14,119,116, 80,194, 77, 78, 82,138,193,110, 83, 60, 74,
 79,116, 60,130,193, 46, 74,194, 83, 82, 44, 80, 53,101, 79, 80, 60, 78, 53,194,
 67, 32,138, 82, 77, 83, 85,116, 79, 80,130, 60, 74, 77, 78,194, 53, 67, 83, 84,
 79,116, 77, 78, 80,130, 53, 60, 74, 83, 67,194,  2,116,130, 80, 67, 74, 60, 82,
 53, 78,194, 77, 83, 93,116, 60, 79, 67, 77,130, 74, 53, 80,110, 30, 32, 18, 67,
154, 74, 77, 82,194, 80,193,130,187,138, 83,129,115,180, 74,124,138,144, 60,187,
 67,134, 32, 53,142,162,194, 67,192, 53, 80,180,187,188, 83, 60, 74, 82,121,116,
 60,174, 74,120,187, 83, 80,124,134, 67,142,119,174,116,194,144, 80, 74,138,134,
142,120,124, 60,101, 74, 80,144, 32, 60,193,134,124,194,138, 82, 53, 22, 60,174,
194,193,116, 80,144, 32, 22,192, 53, 74, 79, 67,120, 74,116,187, 82, 60, 32,174,
 53,194, 83, 93, 67, 74,120,116, 60, 32, 53,187,142, 46, 83,134, 72, 74, 67, 60,
 53,120,116, 83, 30, 14, 46, 80, 82, 85,116,174,134, 83, 74, 53, 60, 67,144, 80,
 82,120, 28,116, 67, 60,120, 53,174, 83, 82,134,194, 74,193, 84,116, 74, 83, 53,
 60,134, 80,144,187,174, 67, 82,123,121,136,188,174, 79, 83,187, 80,180, 77, 78,
 82,194, 72, 30,180, 79, 77,194, 46,188,136, 44, 14, 78,187, 22, 79,174, 80,180,
194,136,187,128,193, 22, 77, 78,115,180, 77, 80,136, 79, 74,128, 78, 83,138, 32,
 82, 26,174,194,180, 79,136,154,128, 80,138,132,187,118, 79,188,180,174,136,194,
 80, 82,187, 83, 46, 44,193, 18, 82, 83,180,136, 77,174, 80,194, 79, 78,154,132,
 65,136, 77, 79, 80, 78, 82, 83,118, 30,138,128,110, 34,180, 82,174, 79,136, 77,
194,188, 80, 78,154,128, 84, 79, 77,136, 78,174, 83,180,194, 80,188, 82,187,  4,
 79,194,174, 77,136,180, 80, 83, 82, 78,187,128,119,174,136, 80,180, 78, 77, 82,
193,194,110,128,138,174, 72, 74,166,167,123,255,255,255,255,255,255,255,255,121,
160,129,123,166,158, 67, 74, 79, 77, 80, 95, 83,115, 79,180,195,166,158,160, 88,
 53, 74,143, 32, 80, 65,123, 67, 79, 95,129, 30, 44, 88, 60, 74, 22, 53, 93, 46,
166,158,129,160, 79,123, 22, 32, 80, 67, 88, 79,123,129, 46,160, 79,166, 80,143,
158, 67, 30, 74,101, 30, 79,123,129, 53,166,160, 67, 46, 74, 88, 22, 16, 60,192,
153, 67, 74,123, 30, 79,129, 46, 82, 53, 28, 79, 60,129, 67,123, 30, 32, 74, 80,
 95,153, 82,  4, 79,129, 60,123,153, 30, 95, 67, 74, 46, 88, 80, 18, 79,123,166,
129,167, 74, 83, 67, 77, 46,143, 80,119,123, 79, 30, 32, 74,129, 80,160, 95, 82,
 46, 67, 74,115,180, 72,129, 81,143,174,123, 60, 67,125, 53, 71,121,129, 73,174,
 72,180,125,143,193,123, 81, 53, 75,119,174,180,194,143, 72, 81,129,179,123, 60,
125, 67, 72,129, 73,174,125, 46, 32, 76,194, 20, 14,180, 30, 85, 73,129, 72, 76,
125,143,194,123, 71,174,180, 67, 65,129, 67, 72, 73,123,125, 60, 81, 75, 32,194,
 71,101,129, 72, 60,194, 81,125, 53, 75, 76, 67, 73,123, 84, 73, 71, 72,129,125,
143, 76,194,123,180,174, 67, 79,129, 72,125, 73, 71, 44, 46, 26, 20, 76, 75, 32,
 93,129, 72,125, 76, 32, 71,143,180, 73,123, 20, 14, 18,129,123, 76, 72,194,125,
143, 71,180,174, 73, 75, 26,174,180, 60, 73, 26, 81,194, 72,154, 75,193,143, 77,
 22, 80, 79,174, 56,178, 91, 63, 84, 49,194,179,125,119,123,125,129, 80,143, 81,
 82,194, 78,158, 83,193, 26,174, 79, 84, 81,194, 26, 80,154,193,151,123,129,121,
158, 56, 80,174,123,125,143, 78, 79,129,193, 81,115,180,123,158,143, 56, 79, 91,
129, 81,178, 80,125, 16,123,125,129, 83, 80, 81, 84,143, 82, 79, 49, 56, 72,129,
123, 79,125,194, 91, 63, 30, 46, 26, 44, 56, 79, 79,129,123,125, 56, 32, 44, 26,
143, 20, 78, 46, 65,123,129, 79,125, 81, 80, 83, 82, 78, 20, 30, 46,  2,129,123,
143,125, 84, 80, 81, 63, 49, 56, 91, 83, 34,129,123,125,143, 84, 56, 63, 49, 91,
 83,178, 81, 93,123,129, 79,143,125, 30, 91, 56, 63, 84,158, 49, 60,115,180, 81,
194,129,123,174,178, 74,172,143, 53,125,119, 81,143,174, 74,129,194,123,125, 53,
 67,179, 62,121,129,123,174,125,143,193, 74, 81, 62, 61, 53, 67, 72,129,123,125,
 74, 61, 81,143, 53, 67, 30, 62, 26, 65,123,129, 67, 74, 81,125, 53, 30, 46, 61,
 62,143, 79,123,125,129, 74, 26, 32, 46, 62, 67, 61, 81, 20, 85,129,125,123, 74,
174, 62, 67, 53,143, 81, 61,194,101,123,125, 62, 61,129, 81, 53, 67, 74,143,194,
174, 93,123,125,129, 74,143, 67, 14, 46, 22, 81, 26, 53, 84,129,125,123, 74,143,
 81, 67, 53,174, 62, 61,178,168, 74, 67, 53,129,123,143, 81,192, 62,180,125, 61,
188, 74, 61, 62, 59, 81, 53, 67,192,178,154,193,179, 78, 22, 80, 71, 79, 85, 77,
 83,179,174,165,194, 22,193,115,180,123,143, 79, 81,129,125, 80,158,174, 77, 83,
121,158, 80,174,123,125,143, 79,129, 81, 85, 92, 77, 72,129, 79, 92, 85,125, 77,
 30,123, 71, 81,143, 83, 26, 79,174,154, 81, 85, 26, 80,194,193,151,123, 83, 16,
129,125,123, 83, 80, 77, 81, 85, 71, 79, 82, 92,119,123,129,125, 80,143, 81, 82,
194, 77, 83,158,193, 28,129,154, 85,125,123, 71, 81, 83, 80, 82, 79, 77, 34,129,
123,125,178,154, 83, 85, 77, 71, 82,143,174, 65,123,129, 79,125, 81, 80, 83, 71,
 82, 77,143, 92, 79, 79,129,154,125,123, 77,158, 71, 44, 20, 32,174, 93, 79,143,
129, 30, 71,158, 80, 77,123, 92, 81, 83, 83,121,123,158, 80,129,174,143,125, 79,
 78,193, 81, 76,115, 79,143,123, 81,158,125, 80,172, 77, 78,180,174, 79,129, 80,
 82,123,125, 76, 20, 44, 26, 62, 69,143,119,194, 80,143, 81,174,123, 82, 78,125,
 77,158,129, 65,123,129, 79, 81, 80, 82, 69, 77, 78, 46, 76, 30, 72,129, 79,125,
 76, 77,123, 97, 81, 46, 69, 55, 62, 84, 79,158,123,129, 80, 77, 76,174,125,143,
 81, 78, 93, 79,123, 76, 77,158,129, 69,143, 80, 22, 26,125,101,123, 79,143, 62,
194, 81, 80,125,129, 55, 76, 97, 16, 62,123,192,125,129,194,143, 76, 55, 97, 69,
 77,  2,123, 62,129,125, 76, 77, 81, 80, 69, 97,143, 82, 22, 62, 80, 79, 97, 77,
 55,193,179, 69,165,174, 76, 67,121,174,123,125,143,129,193,178, 81, 74, 68, 63,
 60,119,174,143, 81,123,179, 74,194,129, 53,125, 60, 63,115,180, 81,123,172,129,
143,174,178, 74,194, 60,125, 72,129,125, 74, 63,123, 81, 68, 60, 53, 69,143, 30,
 22, 60,174, 53, 63, 69,194,179,125, 22,193,178,143, 16,123,129, 60,143,125,192,
 74, 53, 69,174, 81, 68,  2,123,129,174, 60, 74,125, 63, 81, 68,143, 53, 69, 28,
123,129,174, 60,125, 74, 69, 53,143, 81, 68, 63, 85,123,174,129,125, 74, 68, 63,
 69, 60,179,143, 81, 79,123,129, 74,125, 46, 32,174, 60, 53, 68, 26, 63, 84,123,
125, 74,129,143, 63, 81, 53, 60,174, 69, 68, 93,123,129,125, 74,143, 63, 68, 69,
174, 53, 81, 32,

 69, /* c3-g3 */
 77,121, 56,125, 26, 32, 22, 46, 44, 14, 79, 80, 78, 82,119,194,125,123,129, 30,
 82, 26, 14, 22, 78, 81, 32,  2, 79, 83, 78,125,129, 44, 22, 84,123, 46, 82, 32,
 16,125, 81, 46, 83, 32, 82,123, 44, 78, 26, 30, 14, 83,123,125,129, 46, 20, 22,
 49, 26, 30, 32, 44, 83, 90, 79, 30, 32, 81, 20, 26, 78, 22, 49, 14, 91, 63, 26,
 26, 79,154,125, 46,123, 30, 84,129, 63, 32, 44, 63,129,125, 32, 79,123, 81, 83,
 82, 80, 26, 20, 30, 18,123, 26, 46,154,125, 22,129, 30, 63, 56, 32, 49, 22,125,
 79, 46,123, 81, 63, 82, 83, 22, 30,129, 84, 68,129, 32,123, 79,194,125, 20, 26,
 49, 30, 80, 63, 67,123,194, 32, 81,192,178,255,255,255,255,255,255, 32,115, 86,
 24, 73,129,194, 84, 85, 80,125, 38,143,123,121, 86, 40,  0, 26, 73, 84,125, 24,
 80,129, 94, 85,119, 86,194, 24, 38, 73,129,125, 85, 84, 80,  0,178, 65,129,125,
 84, 38, 40, 85, 24, 86,143, 26, 20, 80, 16, 86,125, 38, 73, 26, 80, 84, 85, 94,
 24, 40,129, 63,129, 86, 85,125, 38,143, 40, 24, 84, 26, 80, 20, 22, 86,125, 40,
 38,129, 24, 73, 26,143, 80, 84, 85, 68, 86,129, 80, 85, 73,125, 24, 84,143, 40,
 26,178, 28, 86,125, 85, 73,129, 40, 80, 94, 24, 26, 84, 38, 66, 86,129,123,125,
 80, 24, 38, 85, 84, 73, 20,194, 26, 86,125, 73, 84,129, 40, 85,154, 80, 26, 24,
143, 64, 86,129, 59,123, 26, 24,125, 38,143, 40, 20, 84,125,121, 77, 74, 80, 79,
 22, 44, 60, 14, 30, 78, 83, 46,148, 74, 53, 32, 82, 67,192,130, 22, 83,138, 46,
 79, 65,116, 30,130, 74, 67, 46, 32, 77, 80, 78, 79, 14,119, 67,194, 77, 80, 78,
 30, 83, 22, 14, 46, 82,187, 68,116,154,192, 32, 78, 74, 77, 80, 67, 79, 30, 14,
 18,154, 78, 32, 67,193, 30, 79, 22, 46,194, 77, 74, 64,116,130, 67, 74, 80, 32,
 82, 22,138,110, 79, 53,115, 77, 67, 83, 80, 30, 22, 14, 82, 46, 79, 32, 78, 66,
 74,116, 80, 32, 77,130,194,188,138,110, 79,187, 26, 60,154, 30, 32, 79, 14, 22,
 46, 53,192,194, 77, 63,116, 74,130, 60, 67, 32, 77, 80,138,110, 53, 82, 62,154,
 30, 32, 77, 46, 60, 80, 67,192, 78, 83, 53, 79,121, 32,125, 44, 46, 86,178, 26,
 80,192,129,123, 93,148, 83,123, 80, 78, 81, 82,125, 44, 77, 46, 65, 30, 16, 32,
125, 81,123, 46, 82, 77, 83, 78,194, 26, 72, 65,129,123, 72, 32, 44,125, 81, 20,
193,194, 80,192,150,123, 81, 83, 32, 77,125, 44, 78, 46, 65, 86, 80,164, 30, 65,
 80,125, 32, 81,129, 26, 44, 77, 83, 46,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255, 80,121,125, 87,129, 46, 30, 22, 44, 26, 32, 73,
 79, 14,115,125, 87, 77, 30, 46, 32, 78, 83,123, 79,143,194,119,194,125,123, 30,
 87, 82, 14, 83, 81, 77, 78, 22, 67,123, 81, 32,192,194,255,255,255,255,255,255,
255, 16, 87,125, 81, 73, 46, 82, 26, 83, 77, 78, 32, 22, 65,123, 73,129, 30,125,
 79, 87, 81, 83, 82, 77, 78,148, 83,123, 78, 79, 81, 82,194, 77, 87,125, 44, 46,
 68, 87, 32,123, 30, 79, 73, 26,125, 77, 20, 83,193, 63,123, 73, 87, 79, 77,125,
 81, 83, 82,129, 78, 44, 83, 83,125, 87, 32, 77, 20,123,129, 22, 46, 79, 26, 66,
123,125, 87, 30, 79, 32,129, 81, 77, 78, 83,174,  4, 87,125, 79, 81, 77, 32, 73,
 26,129, 83, 82,123, 46,115, 77,125, 74,182,183, 78, 80, 82,143, 60, 14, 30,121,
129, 74, 79,184, 77,125, 83, 80, 40,123, 78, 22, 63, 38,129, 79,194,123, 77, 74,
174, 30,125,143, 80, 65,129,125, 30, 38,194, 79, 40,123, 74, 53, 60, 67, 67,125,
129,194,123, 30, 22, 38, 74, 14, 40,143,182,119,194, 40,123, 74, 30, 82, 83,125,
 22, 14, 78, 38,  2,129, 30, 74, 60, 40, 22,125, 82, 79, 14, 53,192, 64,129, 79,
194, 22, 38, 74, 80,123,125,174, 83, 53, 18,154, 74, 67, 77,125,182,129, 30, 79,
178, 82, 14, 68,129, 40, 79, 74, 14, 30, 22, 77, 83,125, 38,123, 66, 79, 74, 38,
 30,129,125, 40,123, 14, 53, 60, 80,  4,125,129, 79, 30,192, 60, 38, 77, 74, 14,
 82, 40,123,115,136, 77, 82, 83, 78,132,193, 80, 79, 46,187,128,121,136, 46, 79,
 30, 44, 80,187, 22,193, 14, 83, 82, 16,138, 79,192,136, 83, 46, 77, 82,132, 30,
193,128, 62,154, 79, 77, 83, 30,136, 78,192, 82, 80,118, 22, 22, 82, 22,132, 77,
 79,138, 78, 83,136, 30, 80,128, 26,154, 79,180,194, 77,187,136,138,132, 83, 78,
 82, 67, 30, 79, 78, 80, 82, 83,193,154,192, 77,118,187, 83,154, 77,187, 79, 78,
136, 82, 30,192, 80, 22,194,  4, 79, 82, 83,138,132,154,193, 77,136, 80, 78, 46,
 66, 79, 83, 78,136, 30, 82, 77, 80, 32, 74,193,128,119, 30, 83,136, 77,194, 80,
 46, 78,193, 82, 14, 22,  2, 79, 82, 83,194,193,136, 77, 30,132,138, 22, 14, 30,
121, 79, 74,125, 24, 80, 12, 42, 82, 14, 18,  6, 36,115,125, 80, 46,185, 79,182,
 83, 36, 42, 24,183, 12, 63,123, 38, 36, 79, 80,125,174,184,143, 82, 83, 14, 64,
123, 79, 36, 22, 38,174, 74,184,125, 46, 80, 18, 65,129,125, 80, 79,123, 38, 74,
143, 46, 60,184, 83, 83,154, 79,123,125,182,184, 82, 22,143, 14, 24,  6,148,123,
 24, 53, 67, 74, 46, 22, 38, 12, 36, 14, 42, 67,123,125, 79, 42, 36, 38, 24, 22,
 46,184, 14,143, 18,154,184,182, 24,125, 82, 79,178,123, 42, 22, 14, 66,123, 79,
 24, 80, 38, 18,125, 14, 46, 83,  6, 12,  4,125, 79, 60, 24, 82, 36,184, 18, 42,
192, 38, 46, 22, 79, 60, 24,125,184, 46, 22,  6, 42, 38, 82, 36, 83,115,125, 77,
 76,123,129, 30, 22, 32, 79, 80, 82, 78,119,123,125, 22, 30,129, 14, 76, 46, 82,
 78, 81, 77, 16,125, 81,194, 76,123, 82, 30, 26, 46, 77,129, 78, 67,123, 81,192,
 32,194,178,255,255,255,255,255,255,121,129, 22,125, 46, 14, 44, 26, 80, 32, 79,
 76, 30, 65,123,129, 79, 81, 80, 82, 69, 77, 78, 46, 76, 30, 63,123,129, 77, 79,
 81, 82, 80, 76,125, 78, 69, 44, 64,123, 79, 80, 76, 81, 82, 22,125, 69,129, 62,
 77, 66,123, 79,129, 81, 77, 78,194, 80,125, 30,193, 14, 22,129, 26, 79,125, 81,
 77, 82,123, 22, 32, 30, 46, 28,123,125, 77,154, 76,193,129, 26, 22, 46,143, 80,
 18,154,129,123, 32, 77, 26, 22, 76, 46, 78, 44, 80, 74,115, 72, 22, 76,123, 46,
 32, 14, 67,125, 75, 53, 60,121, 72, 32,125, 26, 30, 22, 46,129, 44, 14, 73,143,
 67,125,194,129, 26, 32, 20,123, 22,192,193, 81,143,148,125,123, 22, 32, 26, 44,
 76, 53, 46, 14,194, 30,119, 22, 32,194, 14, 46, 76, 72, 81,123, 75, 67,125, 55,
125, 32, 20, 22, 71, 46,129, 76, 26, 72,123, 14, 83,129, 32, 72,125, 22, 26,123,
 20, 46, 71, 76, 14,  2, 32, 22, 26, 60, 46,123,125, 76, 73, 75,129,192, 22, 60,
 32, 26,123, 46,125, 22,129, 75, 81, 72, 53, 18, 22, 32,154, 46, 26, 71,192,125,
 72, 73, 14, 75, 28, 32, 60, 26,154, 22,123,125, 46,129,180, 76, 72,101, 32, 22,
 76,125,123, 71,194, 81, 73, 75, 67, 46, 14,121, 79,184,125, 83, 53, 74, 80, 67,
 46,129, 22, 30,115,125,183, 46,182, 30, 82, 74, 67, 38,143,129, 77, 63,123, 77,
 74, 79, 80, 78, 83, 67, 60, 32, 53, 82, 65, 30, 38,125,123, 79, 74,129, 46, 77,
 80,143, 83, 67,125,123, 30, 38, 22, 46,143,194,129, 74,182,185,119, 78,125, 77,
 30, 83, 82, 74,194,123, 38, 22, 80, 66,129, 79, 38, 30,123, 74, 46,125, 83, 77,
 82, 53,148, 74, 60, 46, 22, 38,123,125, 53, 83,143, 30, 67, 64,129, 79,123, 22,
 38, 78, 46, 74,125, 77, 30,143, 18,154, 67, 30, 74,125, 79,129, 22, 38, 78,182,
 82, 83,154, 79, 53, 30,129, 22, 77,143,125, 46,182,123, 62,154, 53, 79, 46, 30,
 74,125,182,123, 38, 22, 78, 22,115,183,125, 74,182, 46, 30, 82, 14, 83,143, 38,
 67,121,125,184, 74, 83, 79, 60, 77, 78, 67, 80, 82, 46, 63, 38, 77, 28, 74,123,
 78, 79, 80,129, 30,143,125,119, 74,125, 83, 30, 82,194,123, 46, 38, 14, 78, 77,
 67,125,123, 30, 28, 74,129, 38, 46, 14,143,183,182, 65,129, 30, 38, 79, 74,125,
123, 46, 77, 80,143, 78, 22, 34, 26, 60,  4, 18,123, 80,183, 79,194, 16,182,148,
 53, 74, 10,123, 60, 46,125, 38, 28,129, 14, 16,  2, 60, 74,125, 30, 46, 28, 79,
 53,182,  4, 14, 77, 18,154, 74, 79,182,125, 28, 77, 30, 78, 83, 14,129,101, 79,
125, 74, 30, 46, 28,194, 38, 14, 77, 78, 83, 68, 79,194, 60, 14, 28, 30, 74, 82,
 78,125, 77,123,
 
115, /* b2-d3 */
129,124,194,124,187,192,193,255,255,255,255,255,255,255, 79, 67, 83, 74,120, 82,
138,180,124, 53, 32, 80, 60, 86,180, 74,124,138,144, 60,187, 67,134, 32, 53,142,
 93, 46,180,188,144,187,120, 67, 74,174,124,142,116,162, 67, 74, 32,180, 53, 46,
 83, 44, 82, 60, 30,192,  2, 67,120,180,187, 60, 74,188, 30, 53, 82, 83,138, 72,
 74,120, 30,116, 83, 53, 67, 60, 82,187,142,124,128, 30,194,193,187,255,255,255,
255,255,255,255,255,100,120, 67,116,174, 30, 83, 46,114,134, 44, 82, 53, 64,120,
180,116,142,144, 74,188,124,134, 82, 67,187,154,180, 74, 30, 22,192, 67,188, 53,
 46, 60, 32,187, 63,180,188,142,120,144, 74,124,187, 46, 67, 44, 60,123, 79,136,
 80, 82, 83,138,187,180, 44, 46,118,110,132, 72,180, 77,187, 32, 30,136, 22, 78,
 46, 79, 80, 44,128, 30,128,178,180,194,187,193, 79,255,255,255,255, 26, 83, 30,
 79, 77,174,180, 80,136,118, 82, 32, 78,  4, 77, 79, 30,136, 83, 78, 80, 82,180,
138, 46,193, 16,136,193, 77,132, 79,110,118, 82, 30, 80, 78,128,124,194,180,187,
193,192,255,255,255,255,255,255,255,  2, 79, 30, 80,136,180, 78, 77, 83,138, 82,
187, 46, 66, 77,136, 82, 80, 78, 79, 83, 30,118,128, 74, 32, 63,180, 80, 79, 30,
187,136, 77, 78, 82, 46, 83, 22, 86,180, 77, 80,136, 79, 74,128, 78, 83,138, 32,
 82, 34, 77,180, 79,136, 30, 14, 22, 80, 78, 82, 46, 44, 77,150,123,125, 49, 79,
 46, 80, 63,143, 78, 32, 83,180,124,194, 26,193,179,192,255,255,255,255,255,255,
255,128, 30, 79,194,178,193,179,255,255,255,255,255,255, 16,123, 32,129,192,193,
125, 56, 46, 79, 78,143, 83, 72, 56,123, 26, 44, 46, 20,129, 30,180, 78, 32, 79,
 86,180,123,158,143, 56, 79, 91,129, 81,178, 80,125, 93,143, 56,125,180,123,129,
 79, 26,158, 83, 78, 46, 26,129,123,125, 32,143, 56, 26, 49, 79, 83, 63, 84, 79,
129,125, 56, 32,143, 20, 79, 46,123,172, 26,158, 63,129, 79,125, 32, 30,123, 26,
 20, 22,143,193,180, 34,129, 56,123,125,180, 79,194, 30, 32,143, 49, 84,  2,129,
180, 56,123, 30, 81,125, 79,143, 91, 78, 46, 78,128, 30,194, 79,178,193,179,255,
255,255,255,255,255,124,194, 26,193,179,192,255,255,255,255,255,255,255, 72, 57,
123,129, 20, 26, 92,125,180, 77, 71, 32, 79, 86,180,123,143, 79, 81,129,125, 80,
158,174, 77, 83, 16,129, 77,125,123, 81, 83, 79, 71, 82, 80,192,143,150,123, 50,
125, 30, 79,143, 64, 71, 80, 46, 77, 32,  2,129,123,125, 81, 79,143, 83, 71, 77,
180, 82, 20, 79,129, 77, 79,143,123,172,158, 46, 71,174,193, 92,168, 85, 81, 79,
 71, 92, 80,180, 82, 50, 57, 83, 64, 64,129,125,123, 71, 85, 77, 32, 80, 22, 82,
 83, 92,102,123,129, 79,194, 77, 71, 32, 80, 14, 81, 30, 22,120,123,129,194, 81,
125, 77, 79, 20, 22, 80, 32, 83, 79,128,180,178, 30,194,193,179,255,255,255,255,
255,255,124,194,192, 26,193,179,255,255,255,255,255,255,255,  4,123,193, 77, 32,
194, 26,129,125, 72, 86, 81, 93,  2,123,129,193,194, 72, 32,125, 86, 93,143,180,
 20, 16,123, 77,125, 26, 78, 81, 83, 82, 72,129, 80,143, 72,125, 20, 26, 86, 72,
180,158,123,193,129, 44, 83,149,123, 72, 80,180,129,193, 32, 44, 81,192,125,143,
 66,129,125, 32,123, 20, 80,192,194,193, 77, 44, 46, 26,180,129,123,174, 26,125,
 72, 32, 77, 83,143,193,106,129,123, 72, 32, 44,125, 81, 20,193,194, 80,192, 34,
180,123, 32, 77,125,194, 26, 20, 72,129,143, 78,150,123, 30,125, 26, 46, 72, 80,
 77,180,129, 32, 86,125,124,194,193,187,192,255,255,255,255,255,255,255,255, 79,
187, 80, 67, 46,138, 74, 53,180, 60, 83,188, 82, 86,180, 80,138, 78, 83, 77, 79,
116,110, 82, 60, 67,130, 87,130, 67, 69, 68,180, 73, 80,110,178, 59,179, 72, 79,
116, 82,130, 30, 74, 80, 78, 77, 83, 32, 60,106,116, 30,130, 74, 67, 46, 32, 77,
 80, 78, 79, 14,128, 79, 30,178,194,193,187,255,255,255,255,255,255, 63, 79, 80,
180,188, 77,187, 83, 78, 82, 74, 46, 53,120, 79,116,130, 30, 32, 67, 46, 74, 77,
 22, 44,194, 18, 77,188,180, 30, 32, 78, 83, 82, 74, 80, 79, 67, 34, 53,180, 60,
 77, 67, 79,188,130,116, 46, 30, 80, 26, 60, 53, 80,187, 67, 77, 82, 79, 30, 32,
 83,116,180, 72,123, 79, 74, 67, 53, 60, 77, 78,129, 95, 80, 88, 18,129,123, 77,
143, 79,125, 44, 78, 46, 22, 30, 80, 79,143,129, 44, 80,125,123, 30, 46,152,166,
172,188, 16, 67, 60, 74, 88,125,181,188, 78, 77, 79, 80,129,  2, 30,129,123,143,
 77,125, 32, 79, 60, 80, 78, 53, 66,188, 60,192, 95, 77, 79, 67, 80, 53, 74,143,
125, 26, 79, 30,129, 60,123, 77, 44, 80, 78, 32,143,125,  4,125,129,143, 60, 30,
123, 77, 79, 80,188, 32, 53, 64, 79,123,125,129,143, 22, 77, 67, 53, 14, 46, 30,
 93,152,129,123, 95, 32, 79, 46,143, 74, 77,125, 80, 86,129, 77, 32, 80,125,123,
143, 79,152, 88, 74, 95, 63, 79, 32,123,129,125, 30, 78, 80, 53, 95, 67, 60, 74,
 72,129, 32,180, 46, 20,143, 30, 14,125, 26, 73,174, 79, 81,129, 72,125, 53, 60,
 20, 67, 46, 32,174,143, 86,180, 72,129, 81,143,174,123, 60, 67,125, 53, 71, 93,
180, 72, 32,143, 46,123,129, 20, 14,174,125, 22,124, 26,194,180,179,193, 75,192,
255,255,255,255,255,128, 30,180,178,193,194,179,255,255,255,255,255,255, 63, 72,
 46,180,123,143,129, 73,125, 20, 32, 22, 71, 18,129, 72, 71, 67, 76,143, 46,125,
 32,123,180, 73, 34,129, 60,180, 67, 26, 22, 72, 73, 71, 32, 76, 53,  2, 60,129,
 72,125, 76,143, 46, 81, 75, 67, 53,123, 66,129, 22, 32, 60,125, 30, 20, 46, 14,
 44,143,194, 64, 72,125, 71, 44,143, 46,123,129,180, 75, 76, 20, 60,128, 30,194,
178,179,193,255,255,255,255,255,255,255,168, 81, 74, 30,123,143, 46,180,129, 20,
125, 53, 44, 72, 74, 22, 26,129, 61,123, 20,180, 44, 59, 46, 30, 63,123,143,180,
172, 46, 26, 20, 61, 67, 74, 53, 32, 86,180, 81,194,129,123,174,178, 74,172,143,
 53,125, 93,143,180,123, 74,172,178, 46,174, 20, 14,125, 26,148, 53, 67,123, 74,
 30,180, 61,143, 46,125, 62, 14,149,123,129, 67, 53, 81,143, 30,180, 32, 74,125,
 14,110,129, 67, 74, 81, 53,123,125, 20, 26, 46, 22, 14, 79, 81,123,125,129,143,
 20, 53, 67, 74,172, 26, 32, 64,123,125, 22,143, 74,180, 46, 53, 62,172,129, 67,
106,123,129, 67, 74, 81,125, 53, 30, 46, 61, 62,143, 67, 79,129,125, 81,143, 20,
123, 26,172, 60, 53,174, 74,124, 26,194,193,179,192,255,255,255,255,255,255,255,
 72,123, 74,129,180, 26, 20, 46, 30, 22, 53, 68, 32,128, 30,194,178,193,179,255,
255,255,255,255,255,255,155,180, 32, 74, 66, 30, 60,123,143,125, 53, 81, 69, 64,
123,125,180, 22, 46, 74,143, 53,172, 60, 44,129, 16,129,125,123, 60,180, 74, 69,
 53, 81, 68,172,143, 86,180, 81,123,172,129,143,174,178, 74,194, 60,125,168,129,
180,123, 66, 81, 74, 30,143,125, 46, 20, 44, 93,180,123,143,172, 74,178, 46,174,
129, 14, 26, 20,148,123,129,143, 53, 30,180, 74, 68,125, 69, 60, 26, 63,123,180,
143, 46,172, 32, 20, 53, 60,125, 74, 26, 30, 18,129,125,123,143,180, 79, 18, 82,
 46, 67,185, 74, 79, 32, 24,123,  0,129, 28, 22, 74, 60, 82, 10,174, 16, 60,129,
123,183, 80, 38, 46,185,182,184, 18, 74,124,194,192,185,193,255,255,255,255,255,
255,255,255, 63,123,180,143, 79, 46, 12, 18, 83,183,125,184,185,130, 73, 67, 68,
 80, 69, 94,178, 87,180,185,182, 59,  4, 60, 79,123,129,125,143, 53, 38,180,193,
185,183,  2, 60,180,123,129, 53, 79,143,125, 38, 67,185, 74, 34,129,180, 79,123,
125, 60, 53,185,174, 80,183,143, 26, 60,123,180,143,125, 79,129,174, 80, 53, 67,
183, 64,123, 79, 36, 22,180,143, 46, 82, 74,182,125, 83, 72,123,129, 74,125,185,
 67, 83, 60, 82, 79,143, 80, 22,124,194,192,193,185,255,255,255,255,255,255,255,
255, 18, 30,129,180,143, 46, 79, 80,125, 74,123, 77, 78, 93,180, 46, 30, 82, 79,
 74,182,174, 67,183,143, 60, 86, 38,180,193,194,192,178,255,255,255,255,255,255,
 79, 60,129,180, 67,123, 14, 30,125, 53,143,183, 82, 63, 77, 79,180, 28, 46,143,
 82, 74,183,123, 30, 67, 16,129, 60, 74,183, 38,184, 80, 46,185,123,170,178,  2,
 60, 38, 30, 79, 67,143,129,180, 82, 74, 53,123,  4,143, 60, 38, 30, 79, 67, 74,
129, 28,180, 46, 82,168, 30, 38, 74, 46,129, 28, 16,183,185, 53,180,143,102, 30,
129,123, 74,194, 79,125, 77, 38, 67, 82, 78,100, 30,123, 74,129,194, 79,125, 77,
 78, 67, 46, 38,
 
121, /* b2 - c4 */
 79, 18,129,123,125, 93, 26, 77, 44, 20,143, 81, 83, 46, 67,123,192, 32,194,178,
 81,255,255,255,255,255,255, 68,123,129, 86,178, 32,125, 44, 77, 20,143,194, 46,
 66,123,129,194,125, 80, 86, 93, 77, 46,174, 81, 26, 64,123,129, 86,125, 80, 93,
 44, 77,174,143, 83, 30, 63,123, 44,129, 46, 86,125, 80, 93, 77,174,143, 78,188,
 83, 32, 81, 26, 44, 78, 77, 46, 30,192, 80,193,149,123, 81, 20, 32,125, 44,143,
 46, 80, 26, 83, 78, 69, 32,125, 44, 46, 86,178, 26, 80,192,129,123, 93,150,123,
 81, 80, 32, 20, 26,143, 93, 44,125, 46, 83,126,123,129, 72, 32,125, 20, 44,194,
192, 46, 26, 86,155,123, 32, 78, 81,125,129, 44, 20, 26,143, 46, 80, 74, 18,129,
 73, 75, 72, 22, 44, 20, 46, 30, 81, 32, 14,162,143,129,123, 32, 26, 67, 53, 20,
 81, 76, 44, 30, 67,129, 72, 32, 26, 20, 44,125, 46, 30,143, 22, 14, 69, 72, 32,
125, 26, 30, 22, 46,129, 44, 14, 73,143,148, 32, 53,129,123, 26, 72, 81, 20, 44,
 73, 76, 67,  2,129, 30, 22, 32,123, 67, 26, 73,125, 60, 44, 72,154, 44,129, 26,
123, 32,125, 20, 22, 73, 76, 46, 53,108,129,193,179,180, 72,192,255,255,255,255,
255,255,  4,129,123, 67, 73, 32,125, 22, 26, 81, 60, 44, 30, 34,129, 75, 30,125,
123, 67, 26, 81, 20, 32, 22,143, 68,129, 26, 44, 32, 20, 22,125, 30,143, 14, 67,
 75, 63, 72,129, 73, 46, 67,143,125, 76, 60, 53, 30, 44,123,108, 80,193,192,180,
187,255,255,255,255,255,255,255, 18,136, 80, 83, 79,188, 44, 30, 22, 14,194,187,
 46, 69,136, 46, 79, 30, 44, 80,187, 22,193, 14, 83, 82, 67, 30, 79, 77, 80, 44,
 22,136, 14,193, 46,187,188, 66, 77, 79, 80, 22, 74, 82,128, 83, 14,193, 78,110,
  4,136, 83, 82, 77, 79, 30, 80, 78, 22, 14,138,118, 26,136, 80, 83, 77, 30, 79,
 78, 46,138, 82, 14, 22, 64,136, 77, 79, 78, 83, 82, 80, 22,187, 30,128,188, 63,
136, 77, 82, 79, 83, 78, 80, 44, 30,132,118, 46, 68,136, 79, 80,174, 30, 44, 82,
 83, 78, 77, 46, 14,106,136, 77, 79, 80, 78, 82, 83,118, 30,138,128,110,  2,136,
 80, 83, 82, 77, 30, 78, 79, 22, 46, 44,138, 80, 67,123, 32, 81,194,192,255,255,
255,255,255,255,255, 63,123,125, 73, 79, 87, 77,129, 30, 44,143, 32, 20,108,123,
125, 30, 22,143, 20, 46, 44, 14, 26, 87, 32, 68,123, 79, 77, 87,129, 20, 32, 26,
125, 44, 73, 14, 18,123, 79, 73,125, 77, 22,129, 87, 30, 14, 44, 46, 69,125, 87,
129, 46, 30, 22, 44, 26, 32, 73, 79, 14, 64,123, 73, 79,125, 87,129, 77, 22,143,
 83, 81, 46, 66,123, 87,125, 79, 30, 22, 20, 44, 77, 32, 14, 26, 16,123,125, 87,
 30, 20, 26, 44, 73, 32, 22,129, 46, 72,123,125, 79, 87, 73, 77, 30, 81, 82, 78,
 83, 20,149,123, 14, 81, 66, 26, 32, 20, 44, 73, 30, 46, 79,116,123,125, 79, 77,
 81, 87, 78, 82, 73,129, 30, 32,129, 67, 74,120, 44, 80,174,124,116,134,144,142,
187, 14, 18, 30, 22, 46, 44, 74,134, 80, 14, 32,187,120, 83, 69, 46, 83, 74, 14,
 82, 22, 32, 44, 30,116,120,124,108,193, 74, 67, 22,187, 30, 44, 83, 14, 46, 80,
 53, 66, 74, 22, 32,116, 44, 80, 46, 60, 14, 53, 83,120, 63,120,116, 74, 80, 60,
 67, 82, 53, 83, 30, 32, 44,  2, 67, 74, 30, 60,120, 83, 46, 44, 22, 32, 53, 82,
 68, 67, 74,116, 44, 32, 60, 22, 53, 30, 82, 14, 46, 64,120, 74, 22, 44,116, 60,
 83, 53, 80, 82, 46, 32,162, 32, 80,116,142, 67,124, 74, 30, 44, 53, 60, 46,155,
 32,116, 80, 53, 74,142,120, 60,124,138, 30, 22,116, 67,120, 30, 74, 80, 44, 46,
 32,134,144,138, 22, 22, 63, 77,123, 28, 80, 38,129, 30, 74,174,125, 78,143, 67,
123,184, 74,143,182, 30,194,125,174, 28,129,178, 69,125,184, 74, 83, 79, 60, 77,
 78, 67, 80, 82, 46,116,125, 38, 30, 79,129, 74,123, 67, 46,143, 80, 77, 68, 74,
 60,129, 30, 83, 53,125, 67,184, 14, 28, 79,168, 16, 30, 38,129,193, 46, 28, 74,
 67, 53,180,125,162, 30,129, 38,182, 46, 28, 78,193, 74, 67, 53, 83, 18,129, 28,
 74, 80, 83, 79, 38, 77,125, 67,123, 78,148,129, 53, 30, 28, 38, 14, 46, 78,182,
193, 16, 74,112,123, 74,129, 79, 30,125,143, 77, 80, 46, 38, 53, 66,129, 80,123,
125, 60, 38, 74, 77, 78, 83, 53, 14,108,129, 80,185,193,192,255,255,255,255,255,
255,255, 44, 64,129, 74, 79, 77, 36,125,143, 38,123, 32, 80, 78, 63, 79,123, 77,
 80, 78,143, 28,129, 83, 60, 53, 36, 69, 79,184,125, 74, 83, 78, 60, 80, 77, 32,
 28, 36, 67,123, 74,129,184,143,125,174,192,182, 36,194, 28, 66,129, 80, 74, 38,
143,123, 79, 78, 60, 53, 67,125,108,129, 80,193,185,192,255,255,255,255,255,255,
255, 68,129, 74, 79, 77, 83,125,184, 82, 80, 60, 53, 36, 18,129, 79, 36, 28, 74,
 83, 77,125,123, 80, 67, 78,116, 79,129,125, 38, 32, 77,123, 74, 67,143, 80, 78,
 34,129, 38, 83, 28, 74, 77,125,123, 36, 60, 80, 67,126, 79,123,129, 32, 77, 74,
 38,125,143, 80, 83, 78,148,129,123, 53, 28,125, 38, 78, 32, 79, 74,143, 67,125,
108, 80,193,192,187,255,255,255,255,255,255,255,255, 18,194,130, 79, 80,187, 67,
 44,138, 30, 14, 22, 46, 67,116,130, 74,187, 46, 44, 32, 22, 14, 30,110,193, 68,
116, 79, 32, 74, 30, 44, 22, 80, 46, 14, 78, 77, 66,130,116, 77,188, 74, 80, 67,
 79, 53, 60, 82, 22, 69, 77, 74, 80, 79, 22, 44, 60, 14, 30, 78, 83, 46,162,188,
116, 78,130, 30, 53, 67,138, 32, 80, 79, 77,106,116, 30,130, 74, 67, 46, 32, 77,
 80, 78, 79, 14, 34, 74, 30,138,130, 53, 44, 60, 32, 78, 83, 80,116, 26, 30, 74,
194,116, 44, 53, 60, 32,138, 67,130, 78, 63,116, 77, 80, 79, 78, 60, 74, 83, 82,
 46, 32,130,134, 30, 67,130,116, 74,138, 77, 80, 32, 79, 46, 78, 77, 69, 56,125,
 26, 32, 22, 46, 44, 14, 79, 80, 78, 82, 67,123, 32,194, 81,178,192,255,255,255,
255,255,255,188, 83, 32, 81, 26, 44, 79, 30, 78, 14, 22, 80, 46,108,129,193, 80,
192,179,255,255,255,255,255,255,255, 63,129,123,125, 79, 26, 32, 80, 22, 44, 20,
 46, 84,150, 49,123,129, 80, 81, 32, 79,143, 30, 83, 91, 44,148, 78,123, 80, 81,
 32, 79, 20, 26, 44,125, 63, 14, 18, 79,129, 26, 32, 20, 56, 44, 30, 46, 22, 81,
 83, 64,123,129, 22, 56, 79, 80,125,143, 84, 91, 44, 49, 26,129,123, 56, 83, 81,
 49, 44, 79, 32,125, 22, 78, 34,129,123, 56, 26, 81, 20, 79, 63, 44, 91, 84, 32,
112,123,129, 79,125, 30, 78, 80, 14, 44, 46, 32, 20, 67,154, 66,123, 26, 74, 20,
 44, 32, 81, 53,129, 60, 68,162,123,129, 32,194, 30,143, 26, 66, 44, 53, 69, 20,
168,123, 66, 32,129, 20, 69, 26, 74, 44, 81,180,125,108,129,179,193,192,255,255,
255,255,255,255,255,255,148,123, 53, 32, 74, 81, 44, 26, 30, 46, 20, 69,125, 66,
123,125, 60, 53,143, 81, 74,129,174, 69, 68, 44, 64, 74, 69, 60, 68,123, 81, 53,
174, 32,192,178,194,155,123, 66, 32, 74, 81,125, 44,129, 20,143, 30,180, 63,129,
 74,143, 30, 68, 44, 69, 60, 53,123, 81,125,  4,123,129, 32, 81, 74,125, 60,143,
 26, 69, 68, 22,  2,123,129, 32,125, 74, 60,143, 26, 81, 69, 53, 22,149,129,123,
 53, 74, 66, 81, 14,193,125,143, 30,194, 14, 64, 22,123,143, 38, 74, 80,174, 79,
 46,129, 30, 60, 69, 79,184,125, 83, 53, 74, 80, 67, 46,129, 22, 30,168, 38, 30,
  8, 46,  2,193,143,125,129,180, 22, 79,154,  2, 30, 38, 77, 46, 22,182,193,143,
129,125,180, 68,129,184, 74, 30, 67,125, 22, 53, 79, 60, 83, 77, 67,123,184, 74,
143,194, 30,125,129,182,174,192,178, 66,129, 80,123, 74, 60, 38, 83, 53,125,143,
 77, 82,108,129, 80,185,193,192,255,255,255,255,255,255,255,116, 38, 30, 79,129,
123, 67, 46,125, 74, 77,143, 82,162, 30, 38, 46,  2, 77,182,193,143,125,129, 22,
123, 18,129, 83, 79,125, 80, 38,123, 77, 30, 22,143, 74, 26,129, 30, 53, 38, 83,
 74,123, 67,184, 60,125,143, 30, 64, 36, 22,123, 74,178,174,143, 38,  6, 79, 12,
 60, 63,123, 80,129,143, 83, 36, 67, 60, 74, 53, 82,125,108,129, 80,185,192,193,
255,255,255,255,255,255,255, 66, 80,129, 24, 38,125, 14,123,180,143, 74,174, 82,
 69, 79, 74,125, 24, 80, 12, 42, 82, 14, 18,  6, 36, 68, 79, 67, 74,129,  6, 24,
 42,125,123, 12,184, 82, 67,123, 74,192,184,143,125,174,193,182,194, 79, 36, 18,
129, 83, 36, 24, 74, 80,125, 38,123,143,184, 67,168,123, 36,129, 79,143, 12,  6,
 18, 80, 74,125, 24,148,129,123, 53, 79, 18, 24, 38,125, 14, 12, 36,184,154, 79,
 24,123,  6, 36,129, 12, 18, 38, 60,125, 80,162,123, 79, 24, 67,129, 53,143,182,
125, 18,  6, 12,

119, /* b2-a4 */
123,128, 30,128,178,180,194,187,193, 79,255,255,255,255,114,136,118,128, 22,132,
 77, 44,138,110, 14, 46,180, 72,188, 30,180,136, 46, 77,194, 22, 44,187, 14, 78,
  4, 30, 82,136, 79, 83, 80, 77, 78,138,188, 46,194, 16,136, 30,193,192, 82, 83,
 80, 77, 78, 14,110, 46, 18, 83,136, 79, 82, 30,188, 80,174, 78, 22, 77, 14, 66,
 30,174,136, 79, 83,193, 80, 77,188,194,180, 14, 26,136, 30,174,180, 79, 80,138,
 78, 22, 83, 14, 46, 67, 30, 82, 80,193, 83,188,194, 77, 22, 78, 14,180, 69, 30,
 83,136, 77,194, 80, 46, 78,193, 82, 14, 22, 34, 30,136,180, 82, 80,188,174, 77,
 78, 14, 22,118,106,136, 77, 79, 80, 78, 82, 83,118, 30,138,128,110, 74,128, 30,
180,178,193,194,179,255,255,255,255,255,255, 67,194, 32,129, 81,143, 20,193, 26,
180, 14, 46,123, 79,129, 72, 32, 81,143,194, 46,125,123,174, 44, 20, 66, 72, 75,
174, 71,194, 81, 46, 76,123,129,143,193, 93, 46,180,143, 32,123, 72, 81,174, 20,
179, 22,129, 86,174,180,194,143, 72, 81,129,179,123, 60,125, 67, 72,129,194, 46,
 30,180,125, 73, 32,174,143, 53, 60, 63, 72,174, 26, 46,194,180,143, 81,129,123,
 32, 20, 69, 22, 32,194, 14, 46, 76, 72, 81,123, 75, 67,125,114, 72, 71, 73,129,
 75, 22, 46,194, 32,125, 30, 67, 68, 26, 32,194, 22,174, 20, 72, 73, 81,143,193,
 75, 64, 72,174, 46, 44, 71,125, 76, 30, 75, 81, 67,143,194,128, 46, 80, 67, 83,
125, 95, 53, 60,129, 74,143,178,114, 77, 44, 46, 74, 95,129, 83, 22, 79, 67, 32,
 30,106,125, 79, 74,123, 46, 77,129,143, 30, 88,190, 60, 72, 79,123, 46, 74,125,
 44, 95,129, 77, 78, 22, 88,134, 79, 46,125,123,129, 77,189, 30,190,143, 74, 60,
150, 46, 78, 95, 82, 80, 83,129,143,123, 32, 79, 88, 86, 95,125, 79,129,143, 46,
123, 74, 83, 30, 88, 22, 64, 44, 95, 46, 79, 67,123, 30, 78, 74,143, 80, 14, 79,
 46,123, 95, 79,143, 80, 74, 44,129, 30, 67,125, 63, 79, 30, 32, 44, 95, 46, 78,
 22, 80, 77, 74, 67, 18, 79,129, 46, 95,143, 74, 80, 78,123, 22, 83, 77, 93, 46,
 95,143, 44,255,255,255,255,255,255,255,255, 46, 72,123,194, 74,129,125, 30, 80,
 14, 67, 60, 77, 78,128, 30,194, 79,178,185,255,255,255,255,255,255,255,114,125,
194, 74, 77, 60, 53, 30, 67, 22, 14, 38,129,106,129,125, 30, 38,194, 79, 40,123,
 74, 53, 60, 67, 16,129,194, 60,125,123, 53, 14, 74, 67, 22, 78,143, 64,194, 74,
 79,125, 38, 78, 67,123, 83, 22, 60, 53, 86,194, 38,178,192,255,255,255,255,255,
255,255,255, 79,125,194, 80, 74, 38,143,123, 67, 83, 60, 53, 40, 68, 40,194, 38,
123, 30, 82,125, 14, 22, 83, 78, 67, 67,194,123, 22, 14, 30,125, 38, 74,143, 40,
129,174, 63,194, 74, 67, 83,123, 22, 30, 78, 77, 38,125, 80, 69,194, 40,123, 74,
 30, 82, 83,125, 22, 14, 78, 38,125, 79,116, 46, 80, 83, 82,138,110, 44, 74,193,
187,194, 68,116, 67, 30, 82, 80, 83, 77,138, 78, 14, 46,110, 66,188, 82,116,194,
 67,180, 77, 80,138, 83, 14,193, 64, 79, 74, 82,187,116, 44, 67,110, 46, 80,130,
138, 69, 67,194, 77, 80, 78, 30, 83, 22, 14, 46, 82,187,106,116, 30,130, 74, 67,
 46, 32, 77, 80, 78, 79, 14,128, 79, 30,178,194,193,187,255,255,255,255,255,255,
 63,116, 82,187, 78, 83, 77, 80, 30, 22,193,138, 46, 18,130, 46, 82, 30, 32,138,
 14, 80, 83, 67, 77, 74, 72,116, 53, 67,194, 78, 60, 80, 77, 74, 82, 83, 46,134,
 30, 67,130,116, 74,138, 77, 80, 32, 79, 46, 78, 34,116, 82, 46, 83, 60, 22, 14,
138, 67, 78, 80, 77, 30, 72,123, 74, 83, 80, 82,129,143,125, 14,184, 67, 46,114,
 79,125,129,143, 36, 74, 14, 67, 83,184, 80, 22, 79,123, 32,129, 67, 24,  0, 22,
 28, 74, 60,174, 10,106,129,125, 80, 79,123, 38, 74,143, 46, 60,184, 83,148, 53,
194, 38, 74, 46,125, 83,143, 18, 67, 80, 82,155, 24,123, 38, 79,194, 74,125,143,
 12,  6, 83, 18, 16,123,129,125, 60, 67, 14,193, 46, 42, 12, 22, 53, 64,123,174,
 36, 74, 38, 67,  6, 83,125, 46, 42,194, 18,129,123, 67, 80,125, 83, 46, 38, 22,
143, 14,174, 67,123,194, 24,125,192, 36, 38, 42, 14, 22,143,182, 66,123, 24, 38,
174,  6, 80, 82, 36, 42, 12,143, 18, 69,123, 82,194,125,184, 80, 83, 12, 14, 42,
  6, 18, 80, 72,125, 73,129, 30, 78,123, 82,143, 46, 77, 32, 14,128, 30, 79,194,
193,179,255,255,255,255,255,255,255, 16,194, 87, 26,129,123,125, 73, 14, 22, 32,
 46, 20, 66, 87,123,194, 81,125, 77, 30, 14, 22, 20, 26, 32, 69,194,125,123, 30,
 87, 82, 14, 83, 81, 77, 78, 22,148,123, 81, 79,194, 73, 78, 26, 87, 30, 20, 83,
125,114,125, 73, 77, 79, 87, 78, 83, 22, 81, 32, 30,129,154,194,180,123, 87,174,
 20,125, 32, 26, 44,143, 46,106,123, 73,129, 30,125, 79, 87, 81, 83, 82, 77, 78,
134,123, 73, 30, 81, 87, 79,125, 26, 83, 82, 77, 78, 67,123,194, 81, 32,192,255,
255,255,255,255,255,255,155,123,174,125, 79, 81,194, 77, 78,143, 83, 32, 87, 77,
 26, 81,174,123,194,129, 30, 84, 26,125, 83, 78, 82, 18,123,129, 83, 78, 80, 81,
194, 30, 46, 22, 91, 14, 16,123,125, 26, 32, 44, 14, 22, 20, 80, 78,143, 81, 67,
194,123, 81, 32,178,192,255,255,255,255,255,255,150,123, 81,125, 83, 82, 78, 80,
 46, 91, 84,129, 30, 69,194,125,123,129, 30, 82, 26, 14, 22, 78, 81, 32, 68,194,
123, 26, 81,125, 20,129, 30, 83, 32, 80, 44,128, 30, 79,194,178,193,179,255,255,
255,255,255,255, 66,123,194,125, 81, 26, 80, 32,129,143, 30, 91, 20,188, 70, 46,
 83, 44, 81, 22, 80, 14, 78, 26, 32, 82,  2,123, 81, 80,194,129, 46,125, 78, 30,
 83, 82, 22, 72,123,129,125, 80,194, 30, 82, 78, 44, 14, 26, 46,129, 69, 83,138,
 32,142,194, 60, 67, 46, 80, 53,134,116,162, 67, 74, 30, 32, 44, 46, 80, 22, 53,
 82, 83, 60, 79,120, 74,116,144, 80,124,138,134,142,194,187,174,114, 67,116, 32,
142, 53, 74, 60, 82, 30, 22,134, 83, 66,120,194,193,116,138,124, 83,134, 67,180,
 74, 82, 72, 74,116, 67, 60,120, 46, 83, 14, 22, 80, 82,187,128, 30,194,193,187,
255,255,255,255,255,255,255,255, 86,174,116,194,144, 80, 74,138,134,142,120,124,
 60, 93,120,144, 46,116,187,174,134,124, 80,138,142, 74, 18, 30,120, 46, 82, 83,
174,194,124, 74, 22, 67,114, 67,116,194,120,124,138, 74, 80,193, 32,134,142,174,
 68,174,116,120, 67,138,124, 82,142,134, 80,144, 83, 67,128, 30,194,178,193,179,
255,255,255,255,255,255,255, 86,174,143, 81,123,179, 74,194,129, 53,125, 60, 63,
114,129,125, 32, 30, 81, 60, 14, 22, 26, 20, 46, 44,149,129,123, 14, 81, 53, 74,
143, 30,125,180,174,194,  2,123,129,174,125,143, 81, 74, 46,194, 60, 69, 53,148,
123,129,174, 74, 68, 81, 26, 46,194,143, 53, 69, 72,125,129, 74, 63, 22,123, 60,
 53, 30,143, 46, 69, 64, 74,194,123, 22,125, 81, 68, 30, 46, 69, 60, 53, 66,123,
 74,194,174,125, 81,143,129,180, 14, 69, 60, 63,123,143,194, 46, 81,174, 22, 32,
125, 20,179, 53,  4,123,129,125, 60, 81, 68,143, 74,174,194, 46, 69, 16,123,129,
194,193,125, 60, 30, 22, 46, 53, 14, 74, 78,128, 30,194, 79,178,193,179,255,255,
255,255,255,255,188, 83, 81, 46, 32, 22, 30, 79, 14, 77, 80, 57, 71,150, 50,123,
 71, 81,125,180,194, 83, 46, 82, 80, 64, 16,125,129,123, 14,143, 22, 46, 80, 71,
 81, 77, 83, 68,194,123, 81, 77, 32,125,129, 44, 83, 80, 30, 22, 72,125,129, 80,
 71,123, 82, 44, 30, 14, 32, 46, 77, 69,125,123, 14, 32, 82,129, 30, 46, 22, 81,
 77, 83, 67,194,123, 81, 32,192,178,255,255,255,255,255,255, 79,123, 77,129, 46,
143,125, 32, 44, 20, 26, 92,194, 66,194, 71, 81, 77,123,129, 30, 14, 80, 32, 44,
174, 18,129,123, 83,194, 71, 80, 81, 46, 22, 30, 77, 14, 63,194,125,123, 77, 30,
 32, 44, 20,129, 81, 46, 83, 82,128, 30, 79,178,194,193,179,255,255,255,255,255,
255,114, 77, 75, 79, 78,129, 83, 80, 61, 81,143, 68, 89, 79,123,125, 80, 81,143,
 83, 46, 20, 44,129, 26, 68, 72,125, 80, 30,129, 78, 89,143, 77, 61, 54, 81, 14,
 66,194, 75,125, 89, 81, 77,123, 30, 80,143, 14,129, 16, 61,129,123,125, 89, 81,
 75, 80, 46, 78, 14, 77, 64,123,125, 89, 78, 80, 83, 68, 81, 75, 54, 77,143, 26,
 81,129,125, 83, 26,123, 77, 78, 80, 89,143, 30,106,123,129, 79, 81, 75, 80, 83,
 68, 78, 77, 89, 54,134,123, 79, 81, 80, 83, 75, 68, 89, 77,129, 78,125, 63,123,
 30,125, 77, 78, 83, 26, 80, 81, 22, 20, 44, 67,123, 81, 32,194,178,192,255,255,
255,255,255,255,

254	/* 7 branches unused */
};

unsigned char entries_6x6_6plies[55986] = {
 86, /* c3-c6 */
 80,115,180, 22,123, 81, 32,152,125,129, 72, 73,123, 79, 77,143, 44, 79, 32,123,
166,125,143, 20, 26, 32, 26, 79,125,123, 30,  2, 77,123,125,143, 81, 73,  4, 30,
 77,123,125, 81,143,123, 22,180, 87, 73,174,136, 79, 26,174, 87,180, 73, 26,128,
 28,136,174,180, 87, 81, 73, 72,180,188, 73,187, 77, 20,  4,174, 77,136, 87, 73,
 81, 84,174,180,136,187, 87,188,158, 65,172,152, 79, 26, 87,166, 72, 73, 79, 82,
 87,166,193, 93, 79,152, 32,123, 87, 30,100,159,166,152,255,255,255,124, 26,159,
152,166,255,255, 79,125, 79,129, 32,123, 26,143,124,194, 26,179,193,187,255, 22,
194, 79,187,180,128,174, 72, 73, 20,134, 26,180,188, 65, 77,128, 87, 73, 20,180,
 26, 87,180,128,172, 79, 26, 16, 83, 77,194, 81,180, 73, 79,128,180,178, 30,194,
179,193, 79,129,125,172,158, 20,174, 16,125,129,180,123,174,172,150,125,123, 80,
 77, 81, 30,155,129, 81, 80,125,192,158,148,192,172,174,129, 78,125,125,124, 26,
194,179,187,193,192, 22,116,180,193,187,165, 79, 16,180,116,193, 79, 73, 78,128,
 79,194, 30,187,193,179,  4,116,193, 81,165,180,194, 28,116,180,187, 81,193,165,
 22,123,115,180, 87, 73,174,136, 79,121, 59,174,136, 73, 79,118, 87, 87,180,179,
194,193,255, 10,174,136, 79, 77,180,128,149,174,128,180, 79,110,118, 84, 59,136,
 79,180,188,174,174,115, 79,153,123, 73,172,143, 72, 26, 73,181,166,167,123, 16,
 87,123,181, 32, 79,173,121,123, 26, 73,129, 79, 81, 30,129,123, 30, 26, 32, 79,
 85,123, 79, 73, 78,129, 87,125,115,116,180,193,187,165, 79, 65, 73,116, 81, 22,
 79, 87,149, 79,180,158,130,110,172,162,116,180, 87,110,130,138,119,116,138,130,
165, 73, 87,101,116,130, 79, 73,138, 87, 87,115, 25,123,129,180, 15,165, 72,123,
 86,125,129, 84, 85,121, 25,123,194,174,143, 80,149,129, 80,180,172, 23,123,148,
  7, 25,123,129, 80,192, 10, 25,174, 37, 80,158, 15, 73, 87, 87,179,194,193,255,
255,121,174,125,123,143, 22,129,115,123,172, 72,174,165,180,119,174, 80,123,143,
 72,165,101,123,125, 80, 71, 72,143, 30,123,151,194,125,174,143, 79,121,174,123,
129,125,143, 80, 30,174,123, 73,143, 74, 72, 79,129,123,125, 87, 84,174, 16,129,
178,125,123, 83, 80,150,172,123, 80,129, 81, 83,164, 80,123,129, 86,143, 81, 72,
 73,115,180, 30,123,129, 32,143, 79,129, 72, 80,123,158,143,148,129,123, 75, 76,
 30, 80,  2,129,123, 72, 30, 87, 74,119,123,129,125,174,143, 74, 16,129,123,125,
 74, 30, 76,125, 74, 32, 81,130,192,180,194,168, 73,130, 32, 79, 87, 44, 75,116,
 87, 77, 81, 14, 79, 71,116, 73, 79, 78,130, 87,148,116,130,138, 87, 73, 79, 65,
116, 87, 30, 73, 81, 77, 79,119, 72,194,143, 86, 80, 82,148,129, 72, 83, 82, 80,
 81,115, 20,125, 86,180, 72,158, 22,123, 72,129,125, 83,143, 28,129,123,125, 32,
 72, 26, 16,123,129,125, 83, 32, 20, 87,115, 37, 86,123, 43, 19,129, 22,123, 86,
125,129, 84, 85, 26,123, 86,194,129,193, 37,119,123, 86,129, 94, 37, 25,149, 80,
 84,123,125, 86, 19,148,123,129, 23, 80, 37,125, 77, 16,123, 79, 83,129, 26, 32,
 26,129,123, 79, 83, 26, 81,119,123,129,125, 80, 30,194,115, 56,123, 26, 44, 20,
 46, 22,129,123,125, 22, 83, 81, 70,129, 79,125, 30, 32, 26, 30,121, 38,158,143,
123, 79, 87,115, 73, 24,123,125, 83,129,119,123,125, 73,143, 82, 38, 74,180,194,
 81,192,185,255, 16,123,125,129, 81, 73, 83,148,129, 73, 83, 38, 87, 81, 65,123,
121,136, 73, 77, 20, 30, 26, 67,174, 73, 81, 30, 26, 79,115, 79,136, 77, 73,188,
 87,119, 73,174, 30, 79,136, 81, 66, 73, 30, 79, 83, 78,136, 69, 77, 79,136, 83,
 87, 73, 73, 67,123, 32,192,194, 74,178, 79,129, 72, 80,123,158,143,121,123, 26,
 74,125,129, 30, 66,123,129, 26, 22, 87, 74,119,123,129, 74,194,125, 46, 69,125,
 32, 22, 46,129, 26,129, 67,116,194, 32,192,255,255, 26, 26, 82, 75, 30,174, 96,
162, 68,138, 26, 75, 96, 46, 69, 68,120, 75, 44, 61, 54, 68, 75, 96, 32,174,116,
192,101, 68, 96, 75, 61,138,194, 30,115,129, 79, 73, 24, 81,123,121,123, 79, 73,
129,125, 38, 72,125, 73,129, 24, 79,143,119,123,194, 73,129,184, 81, 79, 24,129,
 40,123, 79, 22, 67,123,194, 81,192, 18,185, 79,121, 77,123, 32, 86, 44,129,119,
123,194, 81, 72, 86, 78, 72,129,123, 72,125, 32,158, 18,129,123, 83, 77, 32,194,
 26,129,123,174, 32, 72,125,  2,129,123, 32, 83,174,194,125,121, 73, 87,130, 26,
 32, 44,148,138,110, 87, 81, 82, 83, 72,116, 30, 79, 73,130, 83, 69, 87, 79, 78,
192, 77, 32,119, 73, 78, 81, 82, 77,116,149,116, 82, 81, 73,138, 79, 26,174, 32,
 79,123, 32,181,129, 73, 10,123, 79, 30,129, 87, 73,115, 79,123, 77, 87, 73,153,
 18,123, 79, 87,129, 26, 22, 72, 73, 79,129,166, 22, 30, 38, 59, 79, 52,129, 73,
153, 79,121,174, 81,123,143,125,151, 79,129,174, 87, 84,125,143,164, 80,129,154,
 81,125, 51,157, 80,123,172, 26, 81,154,148,174,123,129,158, 81, 80,150,172, 80,
 81,129,123, 86, 87,115,174,123, 25,143,180, 15, 72,123, 86,194,129,193, 37,121,
174, 25, 19,123,194,129,149,180,143,174,125,123,129, 10, 25,174, 37, 80,158, 15,
101,174,143, 25,194, 19,129, 73,121,174,123,125,143, 74,151, 87, 87,193,194,255,
255,255,115,174,172,123, 72,180,143,101,123,194, 75,125, 87, 26,119,174,194,123,
 72,151,143, 85,174,123,125, 87, 72,143, 26, 22, 38, 87,192,194,179,255,115,180,
 79,123,172,174, 18, 14, 38, 87,192,194,179,255,168,123, 73,174,129,180, 59, 30,
 38, 87,192,194,179,255, 44, 38,193, 87,192,194,255, 81,121,123, 60,174, 74,143,
125,115,174, 60,180, 79,123,129, 10, 80,123, 77,125,129,174, 72, 60, 74, 53, 67,
 79,123, 18,129,123, 77, 82, 80, 83,101,123,125, 79,129,174,143, 93, 79,121,123,
129,158, 86,178,125, 79,129,123,158,125, 46, 44, 16,129,125,123, 81, 32, 86,150,
 83,123, 81,125, 82,129,148,123, 30, 83, 81, 80, 77,164, 30,123, 80,143, 81,129,
143, 65,138,128, 30, 87, 73,130, 72, 73,130, 79, 30, 78, 77, 86,128, 73,158,138,
 77,194,115, 77, 79,180,187, 73,172,148,138,134, 82, 81, 83, 20,119,174, 79, 73,
 82, 77, 83, 87,119, 25, 15, 86, 23, 37,158,121, 25,123, 84,125, 85,158,149, 84,
123, 86,129, 85, 80, 79, 85, 84,125, 86,129, 80, 86, 25,174, 37, 80,158, 15, 72,
123,129, 37, 13,125, 86,123,121,136,188, 73, 79, 87, 46,115,136, 73, 77, 46, 20,
187,119, 73,188,180, 46, 81, 82, 86,174,136, 79, 77,180,128, 72, 73, 79,180, 87,
188, 77, 65,136, 77, 73, 79, 30, 78, 30,121, 79,158,123, 22, 73, 38, 86,192, 38,
194,193, 87,179, 28, 79,125,143,123, 24, 22,119,123,125,174, 82, 46,179, 26,123,
 79, 38,125, 81,143, 97, 79,158, 24,184, 46,125, 73,115,123,180,143, 72,158, 46,
 79,129, 72, 80,123,158,143,121,129,123,143,158,125, 46, 34, 72, 87, 74,143, 76,
151,119,123,143, 46, 72,158,174, 97, 46,123,129, 87, 72, 26,123,121,136,149,174,
 80,121,188, 79,123,162, 78, 80, 31,121,187, 13,163,123, 74, 80,121,187, 31,155,
121, 78, 74,178,123, 80,150, 19,178, 80,121,123, 37,255,255,255,255,255,255,255,
188, 28,153,136,167,174,187, 88, 16,153,136, 78,174, 77, 88, 85,136, 79, 77,180,
160, 88, 79,136, 80,187, 46, 44, 83,149,138,160,174, 79,180,128,  4,136,153,174,
187, 88, 79,174, 87, 79, 25, 21,195, 29, 45, 93,136, 95,158, 80, 32,166, 79, 80,
160,188,167,136,158, 22, 80,136, 79, 82,128, 88, 28, 80, 79,136, 82, 77,128,108,
 80, 32, 77,136,180,160, 79, 87,180,194,193,255,255,255, 79,136,188, 87,174,110,
118,149,180, 86,194,174, 30,138, 18,136,188,174,138, 80,128, 22,136,174,128,188,
180, 80, 28,174,136, 83,180,128, 82, 83, 87,193,180,194,255,255,255,108, 80,180,
179,187,193,192,155, 78,174,180, 80,178, 81,  4, 69, 79, 62, 55, 77,136, 72,136,
 79, 81, 69, 76, 62,136,178, 29, 15, 78, 90, 77,187, 87,136,195, 88,194,193,180,
108,173,180,132,128,181,159, 84,136,159,188, 88,166,180, 18,152,136,188,166, 88,
180, 85,136,166,159,152, 88,180, 93,136, 95,159,128, 30, 14, 72, 30,115,128,180,
193,187, 74, 38, 73,184,188, 80,128,187,194, 71,180, 80, 79,128,194,174, 74,112,
130,116, 24, 79, 22,  4,128,194,136,184,187,193, 28,194,128,182,187,180,184,180,
115, 77, 32, 79, 30, 22,136,101,136, 78, 77,188,194, 79,121, 30,188, 79,136, 32,
159, 16,136, 77, 30, 80, 79,194, 22, 80, 79,136, 32, 78, 77, 79,136, 80,152,188,
 30,174, 79,115,180,128,194,187,188, 77, 74, 32,180,130, 81,192,178, 16, 30, 83,
 26, 81, 20,188,  4, 72,180,188, 82, 20,128, 28, 72,180,128,188, 83, 20, 22, 72,
180,188,174,128, 20, 77, 74, 32,130, 81,178,194,187, 16,180, 83,188, 81, 79, 63,
119,188,180,136, 20, 81, 30, 22,188, 79,180, 22,136, 80,115, 56,180, 32,188,187,
 20, 18,180,188, 83, 82,136, 63,194, 93, 95,188,187, 46, 44,255, 16,188,138,136,
 30,180,187,115,152, 32, 79,180,187, 95, 22,180,136,166,152,195, 95,121,188,159,
187,180,136,166, 74,130,112,136,178, 14,116, 46,115,180,187, 22, 38, 30, 40,121,
 38, 22, 78, 79,132, 80, 18, 30, 82, 83, 38,136,188, 26,180,194, 30, 40, 79, 38,
 16, 30, 38, 80, 40, 34, 14, 79, 80, 38,194,188, 34,180,115,180, 79,152,136, 44,
 80,132,110, 16,128, 30,138, 77,136,118, 87, 21, 79, 25, 74, 45,128, 72, 77, 32,
 79, 30, 22,136,  4, 77,152,136,110,128, 30,  2,136, 77, 32, 79,128, 30, 77,128,
 79,178,180, 30,128,194,  2,180, 80,174,136,128, 84, 16,136, 83,180, 84, 49, 78,
 72, 56,180, 32,188,187, 20,124,180,179, 26,193,194,187, 34,180,136,178,128,174,
188, 80, 22,180, 87, 73,174,136, 79, 26,174, 87,180, 73, 26,128, 28,136,174,180,
 87, 81, 73, 72,180,188, 73,187, 77, 20,  4,174, 77,136, 87, 73, 81, 84,174,180,
136,187, 87,188,136, 72,180,127,131, 33, 74,141, 79,187, 80,180,194,188, 33, 22,
180,187, 83, 79, 82,188, 26,174,188, 77,180, 79, 83, 84, 78,174, 79,180, 80, 77,
 28, 80, 83, 77, 79,187,180, 79, 79,136,174,138,132, 93, 20,155, 65,110,192, 77,
 83, 78,128,178,128, 30,180,187,194, 16, 65, 83,136,128, 77, 78,148,108,114, 78,
174, 65,136,150,110, 83, 80, 65, 81, 82, 78, 22,136, 71,128,180, 79, 85, 79,136,
 79, 77, 20, 26,188, 72, 57, 77, 32,180,187, 81,  4, 77,136, 80, 81, 85,128,  2,
 80,136, 71, 85, 77, 83, 28,136, 80,180, 81, 85, 79, 22, 79,121,136,174,128,188,
180, 80, 79,174,188,136, 84, 20, 93, 30, 72, 74, 73, 30,180,174,150,110,154,194,
114, 80,180, 16,178, 65,174, 82,128, 83,164,128, 65,110,174, 86, 77,174,121, 80,
136, 79, 82,128, 88,115, 32, 79, 77, 74, 80, 82, 87, 79, 25,128, 74,195, 29, 72,
 79,180,153, 30,181,136, 65, 30, 80,136, 32, 79, 77, 38, 79,128, 77, 10, 16, 82,
 80,115,180, 87, 73,174,136, 79,121, 59,174,136, 73, 79,118, 87, 87,180,179,194,
193,255, 10,174,136, 79, 77,180,128,149,174,128,180, 79,110,118, 84, 59,136, 79,
180,188,174,180,121,136,152,166,187, 74, 95,101,136,194, 79, 30,152, 74, 93, 79,
 32, 30, 95, 80,136, 79,136, 80,187,188,152,194, 87, 79, 74, 25, 21, 43, 31, 65,
136, 80, 30, 79,187,194,194,121,136,166,152,180,187,173, 93, 95,187,188, 46, 44,
255, 72,180,136,166,152,195, 95, 65,136, 30,187,180,128,173,115,180,136, 79, 77,
166, 32,101,136, 79,128, 46, 30,138,187, 87, 88,195,180,181,194,136,115,180,136,
128, 79,166, 88, 72, 30,136,180,195,181,152,121,128,136,152,166, 88,180,101, 79,
136,194,128,152, 88, 16,152,194,188,128,132,136, 79,188,121,136, 80,187, 46, 44,
 83, 80,195,194, 80,136, 32, 22, 22, 79,136, 83, 80,128, 32, 28, 79,136,174, 83,
 80,128, 72,136, 79, 77, 83, 32,180, 16,136, 83,128, 32,187,194,180,121,187,159,
188,174,194,136,115,152,136, 44, 80,132,110, 16,136,152, 30,187,166, 80,148,166,
 30,174,136,188,187,101,136,194, 80,188, 32,174, 18,152,136, 30,188,174,187,174,
121, 80,160,188,167,136,158, 80,195,136,188, 95, 82, 80, 16, 74, 80,136, 32, 79,
 88,115, 80,136, 79,180,110,153, 28, 80, 82, 79,136, 30, 32,  4, 80, 79,136, 82,
 30,188,136,115,187, 80,180,194,188, 33,163, 80,123, 31,127,187,180,149, 17, 82,
 19, 37, 31, 13,150, 19, 37, 43, 13, 83, 31,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,194,121,188,187,159,136,152,166, 93, 95,188,187, 46, 44,255,
 16,136,152,187,188,166, 46, 72, 30,136,180,152, 95,195, 65,136, 30,187,180,152,
 95,  4, 79,136, 30,188,187,180, 80,121,136,174,188, 87,138,118,119,136,174,188,
 73, 87, 81, 86,174,136, 79, 77,180,128, 72, 73, 79,180, 87,188, 77,101,136, 79,
188, 87, 73,118, 93,136, 73,188, 79, 77, 30, 26,174,115, 79,128, 77,136, 80, 32,
  2, 32,136, 78, 80, 77, 79, 32, 61, 32,153,180,181, 68, 18, 32,136, 77, 78, 80,
166, 10,136, 79, 77, 32, 80, 78,149, 32, 79,110, 74, 80, 77,194, 93, 46, 95,187,
188, 44,255,115, 79,180, 77,136, 88, 32,  2, 79,136,152,128,180, 95, 20, 79,136,
152,180,128,193, 10, 79,152,136,180, 30,166, 34,136, 79,152,166,180, 30,180,115,
 79, 77, 14, 22, 32, 44, 10, 30,136,152, 79, 80, 77,  2, 79, 80, 30,152,136, 77,
 18,166,152,136, 77, 32, 80, 20,174,136,152, 77, 32, 80,149, 80, 32, 77, 30,110,
 22, 79, 79,136, 93, 87,188,138,132,121,180,136, 81,188,128, 80,164,128,110, 65,
180,136, 80,148,114,108, 51,136,180, 81,150,110,114,154,180,194, 80,255,255,255,
255,255,255,255,136,115,174,188, 77,180, 79, 83,157,123, 80, 31, 19, 74,194,149,
 79, 74, 17, 19, 31,174,163,123, 74, 79,180,187, 83,150, 19, 80,123, 31,194,187,
255,255,255,255,255,255,255,154, 72,136,147,182, 78,189,175, 84, 14, 77,255,255,
255,255, 65,136, 30,128,194,138, 77, 20,136, 79,194,138,128, 30,  2,136, 79,194,
178,189,175, 93,136, 79, 30, 95,182,132, 77, 22, 80,121, 73,125,123,143,174, 79,
115, 79,180,172, 87,174,165, 10,129,125,158,143,123, 87, 14,123,125,174,151, 73,
 87, 85,123,174, 79, 73,129,125, 30,129,123,174,192,125, 73, 79,121,174,123,129,
125,143, 80, 30,174,123, 73,143, 74, 72, 79,129,123,125, 87, 84,174, 16,129,178,
125,123, 83, 80,150,172,123, 80,129, 81, 83,164, 80,123,129, 86,143, 81,174, 72,
 26,166,167,123,181,255,121, 56, 26,123,166, 79, 81,115, 56,123,180, 79, 80,188,
 65,123, 81, 79, 26,129, 63, 16,123, 79, 30, 82,129, 80, 85, 79,129,123, 32, 78,
 22,178, 87,179,255,255,255,255,255, 79, 79,171,255,255,255,255, 72,123, 22,125,
 46, 32, 78, 65,125, 79, 32, 22, 46, 30,119,181, 30,171,194,166, 46, 14, 30, 14,
154,123, 22, 46, 63, 87,179,194,193,255,255,255, 30, 77,123,151,143, 68,125,121,
 56,174,125,143, 22, 68,115, 56,180, 22,165,172,174,101, 84,174, 56,194, 22,179,
119,174, 56,165,194,179, 22, 49, 87,179,194,193,255,255,255, 30, 77,123,143, 56,
 54,125,121, 56,143,125,174, 91, 54,115, 56,180,172,174,165, 84,101, 84, 56,174,
194, 22,193, 38, 84,123,178,192, 53, 55,119,123, 72,188,180,136, 20, 81, 30, 65,
 79,136, 81, 30, 78,188, 26, 81,136,180, 84, 80, 26, 16, 78,136, 83, 81,174,188,
128, 79,178,180, 30,128,194, 85,136, 80,174, 81, 78, 91,125, 79,116,194, 46, 26,
138, 78, 22,116, 80,194,130, 84,193, 65, 81, 78, 80, 82,116, 30, 26,116, 81,130,
 80,194,151,128,178, 79,194, 30,193,179, 16, 81,116, 82, 83,194, 84,129, 79,194,
 46,116, 26,120, 78, 22,116,194, 80,174,144, 84, 65, 84,194, 20,116,120, 30, 26,
174,151,116,194,120, 80,150,194, 91,188,134,144, 78,128, 79,194,187,193, 30,179,
 80, 72,125, 73,129, 30,143, 78, 26,174,194, 81,123,151,125,114,123, 79, 73, 77,
129,125, 65,123,194,125, 87, 81, 73,106,129,125,158,143,123, 87,150,123,125,180,
 82, 83, 78,143, 65,138,130, 30,134, 79, 44, 72, 79,134, 80, 82, 26, 30, 22, 79,
194, 80,128, 84,174, 93, 79, 83, 82, 78,187,128, 79, 79,138,128,134, 44, 78, 84,
 80,174, 78, 79,134,130, 81,128, 79,194,193, 30,178,255, 65,123, 74,125, 78, 77,
 80,106, 80,123, 77,125,129,174, 72, 74,123, 80, 78, 30, 14, 79,123,125, 80,143,
194, 46,114, 79, 77,129,125, 74, 80, 26,174, 32, 79,123, 81, 80,181, 32, 72, 79,
166, 80, 81,129, 82, 38, 79,129, 84, 81,192,143, 10,123, 79, 26, 30, 70, 80,  2,
 79,123, 30, 70, 81, 63, 18,123, 79,129, 30, 70,166, 79,121,174, 81,123,143,125,
151, 79,129,174, 87, 84,125,143,164, 80,129,154, 81,125, 51,157, 80,123,172, 26,
 81,154,148,174,123,129,158, 81, 80,150,172, 80, 81,129,123, 86, 84, 87,194,193,
255,255,255,255,188, 49, 26, 86, 63, 91,193, 72,129,123, 26, 86, 63, 87, 85, 85,
174,129, 91,125, 26,150, 49,172, 86, 91, 63, 56,121, 86, 56,174,123, 26, 85, 81,
121,123, 60,174, 74,143,125,115,174, 60,180, 79,123,129, 10, 80,123, 77,125,129,
174, 72, 60, 74, 53, 67, 79,123, 18,129,123, 77, 82, 80, 83,101,123,125, 79,129,
174,143,194, 93,143, 46, 44,255,255,255, 72, 83, 81,129, 79,178, 22,121, 56, 22,
 46, 14,190,195, 38, 79,192,143,178,189, 81, 34,143, 91, 84, 63,193,178,115,143,
123,125, 79, 26, 56, 26, 14,178,192,194, 38,179,255,121,174, 56,151, 79, 84,125,
 30, 38,178,192,179,194,255, 22,178, 38,194,192,179,255,119,174, 84,123, 80,151,
143, 44,192,178,193,194, 38,255,121,158, 79, 56,143,159, 63, 44, 26, 93, 32,143,
 80,123, 56, 63,108,159,165,151,255,255,255, 84, 56,129,125, 80, 49, 79, 85, 80,
 56,129, 79,125, 32,106,129,123, 79, 80,125, 30, 56,148, 59,178, 70, 84, 63, 91,
164, 63, 91,123,125,143, 57,149,172,180,178,158,174,125,106,123,129,125, 84, 77,
143,162, 59, 63, 77, 49, 84, 91,150,178, 49,172,129,180,143, 80, 22, 73,125,123,
143,174, 79, 26, 81,123,174, 73,125,143, 18,129,123, 79, 77, 83, 73, 16, 73,129,
125,123, 81,174, 34,151, 73,123,125,174,143, 72, 73, 79, 87,125,158,129,174, 72,
166, 26,123,167,255,255,130, 81, 32,167,255,255,255, 22, 56, 26,123,166, 79, 81,
 93,160,166, 56,158,143, 63, 84,129, 79, 56,166,158,123, 79, 79,166,158,129,123,
143,123, 22, 56,174, 79,136, 80, 84, 87,193,194,180,255,255,255, 84,136, 56,188,
174, 79, 78,108,180, 80,179,187,193,192,149, 79,180, 80,174,136, 81, 34,136,174,
188, 56,180, 83,125, 87,193,194,255,255,255,255, 18, 56,138, 80, 78, 79, 81,108,
 80,192,179,187,193,255,149,158, 80,172,178,116, 79, 84,158,116, 56, 80, 79, 83,
 85,116, 56,158, 80, 79, 83,115,180, 72,123,129, 56, 44, 79,143, 87, 21, 79, 80,
129, 25, 45,  4,123, 32, 56, 30, 80,178,  2,123, 56, 20, 30, 81, 80, 22,123, 81,
 32,129,152, 44, 93,129,152,123, 46, 32, 79,123,128, 79,178,180, 30,128,194,  2,
180, 80,174,136,128, 84, 16,136, 83,180, 84, 49, 78, 72, 56,180, 32,188,187, 20,
124,180,179, 26,193,194,187, 34,180,136,178,128,174,188,158, 65, 56,152,172, 79,
 26, 78,100,159,152,166,255,255,255, 72, 56, 79,152, 82, 49,129,124, 26,166,152,
159,255,255, 84, 79, 56,129,123,143,125,106,129,123, 79, 80,125, 30,143, 22,193,
128, 79,180,194, 56, 84,158,172,180, 79,130,193,  2, 79,194, 81, 83,128, 80,150,
 81,172,130,138, 84,178, 16,194, 83,193,180, 56, 81,  4, 81,194,193, 80, 79, 56,
 56,148, 63, 91, 70, 77,143,125,150, 63, 49, 77,125, 91, 84,106,123,129,125, 84,
 77,143,163,123,129,143, 63,193, 91,149, 49, 70, 84, 77, 91,123,255,255,255,255,
255,255,255, 79,128,180,178, 30,194,179,193, 79,129,125,172,158, 20,174, 16,125,
129,180,123,174,172,150,125,123, 80, 77, 81, 30,155,129, 81, 80,125,192,158,148,
192,172,174,129, 78,125, 16,123, 72,180, 83,188, 81, 79, 63,115,136, 83,180, 84,
 49, 78, 65,136, 30, 79, 83,193, 82,119, 78,136, 83, 81,174,188, 24,174,178, 79,
136,180, 81, 48,174, 79,188,136,178, 91,129, 22, 79,194,193,116, 80,174, 65, 30,
 79,120, 84, 56, 91, 72,116, 79,120,192, 91, 63,119, 91,116,194,134,142,174, 79,
 79,116,120, 63, 49, 84,121, 56,116,120,192, 80,174,125, 22,116, 80, 79, 56, 84,
154, 65, 81,193, 82, 79, 32, 63, 72,116, 79, 81, 80, 32, 20, 24, 81,178, 91, 79,
116,194,157,130, 80, 81,194, 82, 91, 79,116, 79, 84, 46, 26, 78, 83,121,178, 62,
 81,174,123, 80, 72,123,143, 79,194, 76, 30, 65,123, 62,193, 69, 76,194, 10,129,
 80,123, 76,125, 69,119,123,194,125, 81, 62, 76,101,192,143, 62,129,123, 76, 80,
121, 73,129,125,123, 81,174,115,180,123, 73, 78, 83, 79, 24,174, 81,129,123,125,
 87, 22,123,174,125, 87, 73, 79, 72, 73,125, 79, 83, 87,129, 65,123, 87, 30, 81,
125, 73, 81,115,180, 60,143,123, 74, 79, 10, 80,123, 77,125,129,174, 72, 60,129,
123, 67,125, 53,121,178,125, 60,143,129,192,119,123, 74,129,125,194,143,101, 60,
123,143,129,194,125,125,115,180, 79,152,116,187, 80,110,138, 72,116, 74, 79,130,
 53, 60, 22,116,130,110, 53,138, 32, 65, 77, 30,116, 79, 32, 80,  2,152,130, 32,
166,116, 60, 26,116, 32, 79, 60, 30, 80, 80,124, 26,194,179,187,193,192, 22,116,
180,193,187,165, 79, 16,180,116,193, 79, 73, 78,128, 79,194, 30,187,193,179,  4,
116,193, 81,165,180,194, 28,116,180,187, 81,193,165,138,124,194,193,192,255,255,
255,128, 79,194,193, 30,178,255,130, 67, 87,180, 80, 63, 69, 72, 74,123, 79, 78,
 77, 83, 79,125, 46,180, 80, 60, 53, 18,180,143, 79,123,125, 74, 77, 22,116, 56,
180,194, 79,165,124,194,179, 26,193,187,192,128,178, 79,194, 30,193,179, 93, 32,
188,116, 56, 26, 80, 79, 32,188,187,116, 26, 20,  4,116, 91,194, 56, 81,178, 83,
124,194,193, 26,187,179,192,128,194,187, 30,193,179,178,  4,116, 81,193, 90, 78,
 97, 72,116, 79, 26, 82, 20, 80,163, 80, 79,158, 78, 81,180,155, 78,110, 79,116,
130, 80, 78, 22,116,180, 79, 22, 80,165, 16,116,180, 92, 71, 85, 79,124,194,179,
 26,193,187,192, 79,116, 77,188, 44, 79, 46,168, 71, 81, 80, 79, 83, 92,128, 79,
178, 30,194,193,179,121,116,149,121, 80, 32,174,180, 77,148,121, 78, 80, 74,178,
 79, 18,121,123,117,103, 80,174,116, 79, 78, 83, 77, 82,112,164,121, 80,187,131,
 74,125,155,121, 32, 78, 80,101,174, 80, 87, 87,194,193,255,255,255, 18, 73, 78,
130, 79,187, 83, 93,116, 87,187, 73,158, 77, 79,138,158,187,116,110, 82,149,158,
 66,116, 73,172,138,168,158, 66,116,138, 79,187, 60,108,187,193,192,179,255,255,
 87,193,194,255,255,255,255,162, 67, 59, 62,188,194,187,163, 67,188,172, 74,130,
 81,148, 59, 53,178,194,187, 74, 84,116,187, 74,193,178, 61,187, 87, 88,194,193,
195,255,255, 85,166,159,152,116, 60, 67, 84,166,159, 60,152, 67,116, 79,116,159,
166, 80,152, 32, 18,116,166, 88,152, 95, 80,163,130,159, 32,152,188,195, 78, 18,
 80, 71, 79, 83, 77, 81, 87,193,194,255,255,255,255,108, 80,192,179,187,193,255,
 26,116, 80, 79, 77, 81,130,149,172,158, 80,116, 77, 79, 22,116, 80, 79, 81,130,
 82, 74, 87,180,193,194,255,255,255, 18, 72, 73, 75,138, 67, 76,108, 72,193,180,
179,187,192,154, 73,194, 75,187,110,138,155,178, 81, 60,130, 53,188,162, 67, 73,
138,180,116,187, 72,116, 18,117,103,123, 80, 22,121,162,188, 82, 77, 79, 46,103,
148,101,129,131, 74, 77, 67,149, 67,131,121,129, 80, 32,164, 80, 30,131,101,129,
121,163, 67,180,101,129, 30, 44, 79, 16, 32, 81, 80,138,194,116,148,116,138, 32,
 81, 80, 20,115,116, 20, 86,158, 26, 72, 65,194,154, 32, 81,130, 26,149,130, 81,
138,180, 80,192, 22,116, 72,192, 81, 78,130, 67,115,116, 26, 20, 30, 74,187, 74,
180, 74, 32,130,187,178, 16, 32, 74,130,138, 81, 44,148,130,138, 74, 65,110,116,
101, 63, 74,130, 68, 81, 69,  2, 63,130, 74,116, 30, 60, 80, 74, 32, 81,130,192,
180,194,168, 73,130, 32, 79, 87, 44, 75,116, 87, 77, 81, 14, 79, 71,116, 73, 79,
 78,130, 87,148,116,130,138, 87, 73, 79, 65,116, 87, 30, 73, 81, 77, 53,  2,130,
116, 74, 60, 32, 26,162, 49, 81,138,130, 51,116,101,130, 74, 81, 67, 54, 60, 16,
 74,130, 81,138, 44,116, 74, 74,180, 32,178,130,192,121,116, 60, 74, 81,130,187,
 60,168, 53, 74,110, 67, 81,130,162, 81,130, 53, 32, 30, 67,148,116,110,138, 74,
 61, 67,101, 74, 81, 67, 53, 61, 62, 74,178, 74, 32,180,187,130, 65,116, 81, 74,
138,130, 67, 65,116, 67,123,127, 22,109, 32,129, 18,123,117,113, 32, 80,103,148,
131,121,129, 79, 74, 67,164,121,125,131,111,101,129,149,121, 67, 30, 74, 77, 79,
255,255,255,255,255,255,255, 30,121,116,130, 79, 74,184, 42,115,116,180, 79,187,
 67,138,119,116, 38,110, 14, 46, 42, 63, 38, 36, 79,184, 82, 80, 72,116, 80, 79,
 74,130, 60, 68,116,184, 24, 79,154, 82,130, 69, 32,117,125, 75, 46, 14,119,121,
 72, 75,125, 73,135, 16,117,125,135, 69, 32,193, 67, 72,125,192, 75, 71, 73, 68,
 72, 32, 73,125, 22, 14,115,121, 72,180,117, 71, 75, 74, 72,116, 30, 73, 67, 53,
 60,121,116, 72,138, 73, 26, 30, 66,116, 20,130,192, 75, 32, 34,116, 76, 73, 60,
 53, 72, 68, 26,116, 22, 75,138,188, 18, 76, 72, 67,188, 73, 22, 46,121, 30, 79,
 34, 22, 14, 38, 72, 74, 79, 67, 80, 53, 77, 18,130, 67,194, 79,138,154,115, 77,
180, 53,116, 30, 79, 66, 38,188,194,116,130, 74,  2, 30, 38, 60,130, 67,138, 32,
115,116,188,194, 86,180, 73,121,116, 73, 86, 84, 85,130,119, 26, 86, 38, 40, 73,
 24, 86,116,154, 48,194, 26, 40, 93,130, 84,116, 85, 73, 86,  2,116,130, 86, 38,
 73, 40,119,116, 18,123,103,117,113,121, 80,164, 80,129,121,125, 74,101,149,180,
174, 80,121, 67, 82,148,121,131, 67,194, 78, 82,155,192,174,121,194, 78, 67,163,
101, 67,180,174, 32,129,194, 93, 46, 95,187, 44,255,255, 72,116, 46, 95, 79, 32,
 82, 34,152, 79, 95,116, 22, 46, 65, 46, 78, 95, 77,159, 79, 79,159,116, 67, 46,
 44, 95,114, 77, 30, 46,152,166,159, 80, 79,116, 79,158,194, 81, 83,154,116,180,
138,130,172,110,128, 79,194, 30,187,193,179,162,180,138, 87,116,130,172, 65, 73,
 78, 81, 82, 77,116, 85,187,158,194,116, 83, 81, 77, 79,116,194, 46, 26,138, 78,
 22,116, 80,194,130, 84,193, 65, 81, 78, 80, 82,116, 30, 26,116, 81,130, 80,194,
151,128,178, 79,194, 30,193,179, 16, 81,116, 82, 83,194, 84, 78, 79,116,194, 46,
 20, 77, 32, 65,194, 80, 81, 71, 77, 82,128, 79,178, 30,194,193,179, 22,116, 80,
 71,130,194, 85,134,116, 71, 80, 79,194, 77, 28,116,194, 81, 82,193, 80, 82, 65,
 81, 75, 68, 78, 32, 80, 72,116, 68, 54, 78, 75, 32, 26, 81,194,116, 80,130, 26,
 79,116,194, 81, 80, 83, 46,128, 79,194,187, 30,193,179, 85,194,116, 75, 83, 81,
 54, 85, 79, 16,116, 81, 72, 82,194, 93,121,116,158, 86, 80, 30,194,148,130,110,
 65,138,116, 81,164,110, 80,130, 86,194, 30,150,110, 86,180,116, 65, 81,255,255,
255,255,255,255,255,116, 18,113,117,123,103, 80, 32,164, 80,121,129,131,101,125,
148,121,129, 67,101,131, 79,149,180, 67,121, 79,129, 82,163, 32,180,101,174,121,
 78,255,255,255,255,255,255,255, 80, 71,116, 73, 79, 78,130, 87,119,187,158,194,
116, 83, 81,115,180, 79,116, 78,187,158,121,158, 73,116,130, 87,110,101, 79, 87,
130, 78, 77, 81,162,116,138,130, 87,172,110,130, 71, 72, 73, 32,121, 69, 97, 26,
 62, 97, 90, 69, 83,125, 22, 62,194, 72,145,121, 73, 18, 72, 62, 32, 71, 75,143,
 86, 72,143, 32, 75,117,194,  4, 72,117, 73,121, 71, 75, 60,101,110, 61, 81,130,
138, 53, 64, 22,116, 67, 74, 26,130,121,116,193,187, 74, 67, 62,168, 74, 53,130,
116, 67, 61,184, 74, 81,130, 67, 61,188,148, 74,138, 81, 67,130,110, 74,121, 72,
116, 73, 76,180, 60,115,116, 71, 72,180, 76, 73,119, 72,116, 67, 71, 73, 81, 64,
 72,116, 71,110, 67, 70, 18, 76, 71, 72, 75,138, 73,101, 71, 81, 72, 73, 60, 53,
129,115,180, 79,116,120, 32,124, 80,134, 72, 74,116,120, 67, 60, 53,  2, 60,120,
 67,166, 74, 32,130,152, 80,120, 44,144,134, 93, 32, 46, 95, 60,138, 67, 85, 32,
 95,116, 53, 60,120, 74,128,180,179, 30,193,194,187, 72,120,116, 32,180,187, 30,
124,180,179,124, 26, 75,193, 16,120,180, 60,116,174,142,162,180, 73, 71, 67,187,
 75,  4, 72,180, 60,120,193,116,138,124,194,193,192,255,255,255,128, 79,194,193,
 30,178,255,130, 67, 87,180, 80, 63, 69, 72, 74,123, 79, 78, 77, 83, 79,125, 46,
180, 80, 60, 53, 18,180,143, 79,123,125, 74,124, 26,123,117,103, 77,180,113, 72,
 67, 80, 79, 78, 77, 32, 79,109,111,180, 80, 67,129, 93, 80, 46, 30, 77, 78,174,
 65,109,111,129, 30,180, 80,124,180,193,194,187,192,255,144,124,194,187,192,255,
255,255,128,194, 79, 30,187,178,255,130, 67, 87,180, 80, 68, 69, 79,135,131, 80,
180, 67, 83, 65,131,129, 77,180,135, 79, 18, 77,180,187, 80, 83, 82, 60,162, 67,
 74, 53, 61,180, 81,148,114, 53,192,180,124,188,124,194,124, 26,192,193,179,128,
194,187,193, 30,179,255,155,180, 74,144, 61,120,124,154,180, 74, 61, 59, 81, 62,
162,194,115, 67, 46,180, 44,166,192, 16, 67, 95,180, 80,188,187, 65, 67, 32, 46,
 44,188, 30, 79, 44, 46, 67, 32, 95, 74, 58, 67, 22, 46, 95, 74, 30, 51, 67, 53,
 46,192, 82, 32, 67, 16, 65,180,172,194,116, 69, 65, 66, 30,180, 22, 32, 46,115,
 66,180,194, 74, 69, 68,168, 63,180, 65, 81,114, 66,154, 63,172,180, 66,194, 65,
 18, 66,180,192, 73, 80,174, 53,115, 67, 50, 74,180, 49, 55, 51, 67, 46, 26, 74,
 81, 60,  2, 67, 74,194, 81, 51, 60, 72, 67,192, 46, 26,194,138, 65, 74, 67, 26,
 51, 46, 20, 26, 67, 26, 49,194,180, 55,192, 65,124, 32,116,144,142,138, 79, 67,
194, 32,142,180, 44, 58, 67, 30, 22, 46,116, 32, 72, 67, 53,138, 32,124,142, 85,
 67,194, 46,138,142,180, 84,142,194, 46, 67,180,124, 80, 22, 87, 32, 81,125,115,
145,121, 66,187,145, 78, 73,125,154,115, 81,143,145,125,139,115,143,115, 66, 87,
139,145, 26, 32,143,145,139, 87,125, 28,143,125, 87, 81,145, 32,180, 72, 67, 95,
116, 74, 32, 88, 79, 32, 95, 67,194,116, 60,101, 74,192,116, 95, 46,138, 51, 67,
124,116,192, 53, 74,121,159,142,144, 95, 53, 74, 58, 67,194, 60, 74, 32,187,121,
116,149,121, 80, 32,174,180, 77,148,121, 78, 80, 74,178, 79, 18,121,123,117,103,
 80,174,116, 79, 78, 83, 77, 82,112,164,121, 80,187,131, 74,125,155,121, 32, 78,
 80,101,174, 60,162, 67,180, 74,187, 53,188,149,180,174,172, 53,124, 67, 87,193,
194,255,255,255,255,108,165,193,174,187, 62, 67,116,116, 53, 61,194, 67,120, 84,
120,116,134, 62,193, 61,174, 72,166, 74,167,124,255,255, 93, 46,158, 95,160,120,
166, 87,158,160,144, 21, 31,134,116,166,167,116,173,181,255, 79, 67,160,166, 80,
134,158, 16,167, 74,153,166, 60,120,120, 22,121,123,103,117, 80, 22,162, 67,135,
 78,180,129,178,155,115,135, 32,174,129,178,148, 53,178,135,174, 78,129,163, 67,
135,129,174,105,178,149, 53,115, 22,135,180, 74, 74, 87,180,193,194,255,255,255,
155, 60,174, 75,180,188, 67,162, 67,180, 53,187, 60,138, 16,174,180,116, 73, 60,
120, 79, 72, 60,188, 75,116, 73, 26,120, 72,180, 60,116,144,187,155, 32, 80,159,
152,166,195, 87, 88,193,194,195,255,255, 16,152,166,181,120,116,173,149, 53,180,
195, 32,166,159,116, 67,194,166,120,159, 32, 85,159,120,166,152, 53, 88,119,174,
 93, 95, 46,166, 88,120, 44, 72, 74,166,167,124,255,255, 79, 74, 67,166,160,116,
 82,162, 67, 74,116,180,124, 32, 65, 67, 95, 30, 60,124, 74, 85,116,120,160, 95,
 88,166,116, 18,123,103,117,113,121, 80,164, 80,129,121,125, 74,101,149,180,174,
 80,121, 67, 82,148,121,131, 67,194, 78, 82,155,192,174,121,194, 78, 67,163,101,
 67,180,174, 32,129,144,128,194, 79, 30,187,178,255, 65,131, 14,135, 82, 78, 67,
 79,131,135,194, 46, 82, 44, 72,131,194, 14, 30, 83, 82,114,194, 74, 79, 77, 53,
129, 18, 67,194, 80, 83,131,187,194, 65, 67, 46, 30, 82, 95,159, 72,166,120,116,
 46,159, 30, 93, 95, 46,144, 44,187,142, 87, 31,144, 80, 29, 45,190,114, 67,152,
144, 95,120,166, 79,166, 67, 95, 53, 83,159, 80,128, 79,194, 30,187,193,255, 79,
135, 79,125,117,115,143, 93,125,117,115, 77,143,187,106,117,125, 79, 66,143,194,
 65,117, 66,125,135,121,115,154,115, 87, 32, 81,143,145,138,128, 79,194,193, 30,
178,255, 65,123, 74,125, 78, 77, 80,106, 80,123, 77,125,129,174, 72, 74,123, 80,
 78, 30, 14, 79,123,125, 80,143,194, 46,114, 79, 77,129,125, 74, 80,101, 74, 22,
 71, 72, 22,174,179, 60, 85,120, 72, 73, 71, 76, 75, 72,120, 73, 75,180,124,188,
110,120, 72, 71,180, 73, 75, 87,185,180,179,193,194,255,162, 71, 73,180, 67,187,
 75, 80, 22, 66, 73,125,187, 87, 79,106,117,125, 79, 66,143,194,114,121,117,115,
 79, 66, 77, 26, 66, 79, 87,117, 81,187, 72,117, 87, 73, 30,135, 66,116,143,125,
115, 79,117, 66, 32,116,194, 26, 20, 24, 38, 40,106,116,120,192,187,194,193,162,
192, 38, 26, 40, 24, 80,154,192,188, 24,174, 26, 38,110,116,194,120,124,192,187,
148,114, 80,192, 40, 24, 38,144,114,194, 74, 79, 77, 53,129, 79,131,135,129, 74,
 82, 53, 72, 79,135, 77,131, 30,129,116,131, 77, 80,194, 79,129, 22, 60, 77,194,
 79,131, 53, 26,131, 79,129, 46,194,139, 60,116,116, 53, 61,194, 67,120,162, 67,
 74, 61, 81, 53, 62, 72,120, 74, 62, 30, 81, 61, 65,120, 74, 62,138, 81, 61, 85,
120, 61, 74, 53, 62,124,170, 67, 74, 81, 61, 62, 53,134, 22,194,121, 60, 79, 80,
 32,114, 74,121, 79, 77,129,194, 79,121, 32,129, 80, 83, 82,106, 79, 77,194, 74,
 32, 78,116,121, 77, 80,194, 79, 82, 93,121, 77,129,143, 32, 79, 22, 60,162, 67,
180, 59, 74,194, 53,149,180,174,172, 22,194,144, 10,116,120, 62,174, 74, 61,150,
114, 53, 67, 74, 61,172,148,114, 53,174,194, 74,193,164,124, 67, 53,174, 81, 74,
194,115, 60,180, 53,116, 95, 67, 93, 46,142,187, 95,144, 44, 10, 67,166,152,159,
 95, 53,101, 60, 46,142,116,190, 95,119,152, 95, 60,166, 46, 32,121, 60,152,195,
166, 46,173,174,115, 74,181, 53,180, 67,116,162, 67, 74,181,124,180, 44, 72, 74,
166,124,181,167,255,121,153,120, 60, 74,181,124, 28, 60,120, 74,166,153, 67,  4,
153,166, 74, 67, 60,120,193,115,181, 88,195, 60,120, 32, 93,187, 44,142,255,255,
255,149,194,134,138,190, 53, 80, 38,194,187,192,255,255,255, 72,120, 30,181, 60,
187,194, 16, 67, 32,194, 60,189,181,116,148,131, 79, 74, 80,162, 78,149,131,121,
 79, 80,180,194,164,131, 80,121,129,125, 74,157, 22,103,121,188, 80,194,163, 32,
 67,180,154, 79,194,255,255,255,255,255,255,255, 80,121, 66, 79, 73,125,117,187,
115, 79,187,180,115,188, 22,162, 87, 32, 81,125,115,145,101, 66, 73,125,187, 87,
 79, 10,117,125, 79, 66,143,194, 30,174, 73,117,125, 66,115,174, 72, 74,115,129,
188,180,195, 32, 73, 73,195,188, 46, 32, 26, 20,162,188,180,192,195,160,158,101,
129,188,195,160, 30,166, 93, 32,153,188,160,166, 14, 65, 72,123, 46, 67,129, 32,
166, 75,167,160,158,255,255,255,115,123, 79,129, 74, 67, 53, 65, 67,160,159,158,
255,255, 73, 79,129, 60,125, 80, 32, 16,129,125, 60, 67, 14, 22, 79,129, 79,160,
143, 80, 67,123, 16, 80, 32, 30,153,188,180, 74, 74,180,166,167,255,255,115,180,
166, 74,195,160,158,  4, 74,180,181,153, 30,188,149,188, 74,160,180,195, 79,119,
 79,160,188,153,180, 74,167,115, 74,195, 60, 95, 53,123, 26,166,255,255,255,255,
255, 65,160,159, 67,255,255,255,101,129, 79, 74,123, 60, 44,  4,166,255,255,255,
255,255, 71,123, 32, 79, 30, 60,125,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,121,160,116, 67,159,
166,167,153,255, 79,123,129, 80,125,159,143, 84, 32, 80,129,125,159, 30, 85,129,
 32, 80, 60, 67,125, 93,125,159, 77, 32, 79, 67,126,123, 80, 32, 79, 30, 74,123,
 87, 79, 25, 21,195, 29, 45, 93,136, 95,158, 80, 32,166, 79, 80,160,188,167,136,
158, 22, 80,136, 79, 82,128, 88, 28, 80, 79,136, 82, 77,128,108, 80, 32, 77,136,
180,160,129, 72,166, 74,167,124,255,255, 93, 46,158, 95,160,120,166, 87,158,160,
144, 21, 31,134,116,166,167,116,173,181,255, 79, 67,160,166, 80,134,158, 16,167,
 74,153,166, 60,120,166, 85, 79,159, 78,125,123, 77, 79,125, 46,129,159,123, 80,
 84,129, 77,125, 79, 67,123, 72, 79,160,125,129, 74, 95, 93, 79, 77, 88, 74, 46,
159,108,159,160,165,167,173,255,158, 87,129,160, 21, 80, 31,159, 79, 80,159,123,
 67,160,129,108,159,165,151,255,255,255, 72,160, 79,129, 32, 77, 80, 93, 32, 80,
125,123, 74,129,149, 14,160, 67, 74, 83, 32, 74, 18, 73,129, 32, 46,188,166, 72,
 73,160,129, 26,166, 32,154,123, 73,158,129,143, 95, 22, 60,129, 26,188, 73, 75,
116, 67,166,173,167,181,255, 85,129, 32, 26,166, 67, 46,115, 79, 16,129,188,172,
143,123,180, 79,129,172,158, 32,160,188,128,158, 73, 87,166, 80,188,155, 78,129,
188,158,192,160,162, 78,123,143,129,172,192,148,129,123,192,160, 78,172,180, 79,
129,143, 80, 44,123, 46, 72,123, 74, 79, 53, 67, 60, 22,129,152, 60, 77,143,125,
  2,129,123, 60, 77, 80, 78, 87, 21, 79, 80,129, 31, 25, 93,152,129, 79, 95,123,
 32,195, 79, 46,160,123,129, 79, 80, 18, 79,167, 77,143, 78,129,102, 79,123, 46,
 95,194,190, 16,190, 60, 95,153,181,129, 93,188,143, 95, 44, 46,255, 65, 79, 46,
 32, 30,129,123,166, 72,123, 79,129, 74, 67, 53, 65,180,190, 67,129,125, 30, 79,
 80,180, 32,125, 79,160, 22, 60, 95,180, 67, 79,152, 28, 60,152, 79,180, 95, 67,
  4, 60,125,123, 67,152,180, 80, 72, 73,166,180, 26,123,167, 22, 79,153,123, 73,
172,143,130, 32, 81,167,255,255,255, 65,188,180,123, 20,195,166, 16,188, 79,153,
180,123,181,  4,123,180,188, 79, 73,181,158,124,166,152,159,255,255,255, 65,160,
 88, 95, 67,152,129, 79,125, 80,129, 44, 83,123,100,159,152,166,255,255,255, 72,
 79,123, 67, 74,129, 88, 93, 79,152, 32, 30, 77,129, 65,123,121,136, 30,138,188,
 44, 95, 16,192, 77, 79,118,136, 32,101,188,136,180, 30, 95, 88,115,136, 79,180,
 80,110, 77,  4,136, 79, 32,188, 78, 80, 66, 79, 77,136, 74,192,132, 67,115,188,
180,129, 32,153,123,121,129,181, 30, 81,195, 32, 66,123, 74, 95, 81,192,180, 93,
166,158,160,143,123, 95,101, 30,129, 74,153,166, 26,148,129,123, 32, 30, 46, 65,
 79,121,129, 81,192, 72, 44, 77, 18,123,129, 72,192,195, 26,115,195,188,180, 20,
123,171,101,192,195,169,171,123, 30, 16,129,123,192, 86, 72, 32,  4,123,129,192,
195, 26, 44, 44,101,192,160, 36, 74, 60, 82, 18, 32,129, 77, 67,123, 79,121, 38,
129,160, 74, 67, 79, 72, 74,123,167,166,255,255,115,188, 67,192,180, 74,160,119,
 32, 88, 36, 38, 95, 74, 88,101, 74, 60,195,123, 30, 20,115,180, 20,188, 67, 74,
 60,121, 81, 67,129, 44, 74, 95, 72, 74,123,166, 26,167,255, 18, 87, 74,129,123,
 95, 26, 16, 60,129, 20, 67, 89, 95, 30,121,123,160, 67, 95, 24, 38,101,192,160,
129,195,123, 80,115,180,195,188,160, 67, 24, 72, 74,167,166,123, 24,255, 16, 60,
129,143,166, 79, 12, 18,123,129,195, 18, 24,192, 93, 46,121,129,158,160,166,123,
143, 72, 74,166,123,167,255,255,115,180,166,153, 79,160,158, 65,129, 74,123,160,
 30, 67,101, 79,123,129,166,158, 88, 79,160,195,129, 40,143,123,158, 65,176,152,
 30,160,159,182,115, 79,152, 32, 30, 77,129, 72, 79,129, 80, 32, 74, 30, 95,123,
129,159,143, 30, 88,101, 79, 46,125,143, 67,166,119,159, 79,160,152, 46, 95,166,
 65, 67,160,159,158,255,255,121, 79, 77, 88, 74, 46,159,115, 79,152, 32, 80,143,
 77, 79,129, 79,160,143, 80, 67,188, 83, 79, 30, 32, 67, 80, 16,125,129, 60, 79,
 30, 22,160, 97,166, 83,125,152,159,255,115,152, 79,188, 95,166, 67, 65,129,125,
 32, 46, 60, 30, 72, 79,123, 74, 60, 32, 80,121,125,159, 77, 32, 79, 67,119, 32,
 79,125, 80, 95,123,123,121,136, 95,158, 80, 32,166, 16,188, 95,136,180, 79, 88,
115,180, 79,136, 95, 83, 82, 97,166,180, 83,132,255,255,119,136, 79, 95, 83,188,
160, 22, 79,136, 95, 30, 46,180,129,121, 46,158, 95,160,120,166, 97,166, 83,255,
255,255,255,101,166,120, 74, 46, 67, 88,162,188,158, 67,116,180,124,  2, 95, 46,
166, 60,144, 88,119, 95, 46,166, 88,120, 44, 79,123,121, 80,160,188,167,136,158,
 80,195,136,188, 95, 82, 80, 16, 74, 80,136, 32, 79, 88,115, 80,136, 79,180,110,
153, 28, 80, 82, 79,136, 30, 32,  4, 80, 79,136, 82, 30,188,129,115, 74, 67, 88,
 95,188,180,162, 32, 95, 67,116,188,180,121, 67,160,166, 80,134,158, 80,195,137,
123,133, 80,167, 65, 67,120,160, 30,116, 44, 72, 74,166,167,124,255,255, 46,115,
195,160,158, 38,180,143,121,166,160,158,129,167, 38,101,195,166,123,160, 79,129,
 72, 74,166,123,167,255,255, 65,129, 74,123,160, 30, 67, 80,195,129, 74, 22,167,
 80,160,115, 79, 80, 46, 44,125,123, 65,129,125, 32, 46, 60, 30,121,123,129, 80,
125,159,143, 72, 79,123, 74, 60, 32, 80,101,129, 79,123, 32,125,152,119, 32,123,
 80,129,125, 79, 79,121, 72, 81,123, 32,192,160,148,123,129, 80, 72, 32, 81, 16,
 32,129,123, 80, 81,143,162, 32, 78,143,192, 77,188,164, 30, 80,123, 32,192, 44,
255,255,255,255,255,255,255, 80, 72, 73,166,167,123, 26,255, 65,123, 87, 81,160,
129, 79,115,123,166,160,188,180, 32, 16,129,123, 87, 26, 32,166,121,123,160,158,
 26, 44,166, 93,160, 87,123,158,166,143,

119, /* b2-a4 */
123,128, 30,123,127,121, 74,131, 14, 18, 16, 80, 24, 12, 36,193,128, 66, 80, 24,
 18, 79, 83,128,127,128, 80,174,183,182,193,121,128, 80, 12, 38,187, 36,141,128,
182, 82,174,193, 38,128, 66,133,141, 83, 80,113,143, 68, 74, 82,133, 79,192,178,
 72,188, 80, 79, 30, 46, 44, 79,123,109,105,129,174,188, 18, 74, 30, 83,133,113,
141,114,133,113, 32, 46,188, 79,178, 18,136,184, 30,194, 44, 46, 26,184,194, 83,
136,138, 30, 72,194,136, 88,184, 83,138,113,136, 30,194, 83,190, 95,115,176,190,
 30,175,114, 44, 67,184,194, 30, 82, 83, 46,180, 68,188,174,255,255,255,255,123,
 74, 79,192,137, 95, 80, 18,136, 79, 80,188,187, 44, 72,136, 80, 32, 79, 44, 46,
 16, 80, 79, 95,136, 32, 46,113,136, 79, 77, 80, 30,152,194, 68,138,193,188,195,
 82,255, 18,136,195,187,180,188, 46,123, 79,192,190,189, 83,191, 67,136,191,187,
180,193,188, 72,136,195, 46,180,187, 88,148, 95,138, 88,114,108,187,187, 68,188,
193,195, 82,255,255,123, 74,188,195,193,194,180,143,188,180,193,195,194,255, 16,
193,136,180, 30,194, 44,168,193,194, 88,180,159,132,  4,194,188,136,180,193,118,
114,136,123, 32,123, 22, 24, 28,  0,149,123, 33, 74, 31, 78, 79,150, 19, 83, 82,
121,188, 77,155, 78,121,123, 74, 19, 37,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,118,123,123, 74, 77, 78,131,109, 74,131,109, 30, 22, 46,103,109,
109,123, 67, 77, 60, 79, 71, 79, 67,123,131,109, 77,119,123,131, 67, 80,109, 46,
 73,131, 67,109, 14, 80, 22,128, 74,133,113, 32, 46,188, 79, 73, 79, 80,113, 30,
141,133, 75, 79, 30, 74, 46,113,133,109,123,133, 79, 74, 60, 83,127,123, 79,133,
 74, 82, 83, 91,133,141,123, 67,113, 74, 22, 16,136, 77, 78, 28, 30,138, 91, 77,
 38,136, 79, 80, 78, 18,136, 83, 77, 78, 82, 79, 84,136, 38,180,194,192,193,  4,
136, 83,174, 78, 79, 77, 63, 78, 77, 80, 38, 79, 83,132, 73, 77, 74, 79, 80, 22,
 14,123,188, 74,145,117,180, 78, 74,117, 22, 30, 83, 32, 44,109,123, 79, 77, 60,
 80, 67, 75, 32, 77, 79, 30, 14,117, 91,117,123, 74,145, 83, 77, 77, 74, 32,180,
192,194,187, 81, 16, 30,136,188,132,128, 22,123,129, 22,105,109, 32, 79, 26,136,
 22, 70,128, 26,138,119, 79,136, 81, 30, 78,188,127,136,188, 22,180,128,118, 72,
188,128,136, 30, 32, 80, 74, 83, 16,136, 80, 79, 30, 78, 83, 70,136, 79, 78, 77,
 80, 30,106,136, 79, 77, 83, 32,180, 18, 80,136, 32, 77, 79, 78,114, 80, 30, 79,
 78,136, 74, 30, 74,130,182,112, 14,116, 42,154, 79, 24, 18,188,180,194, 73,128,
174,180,188, 38, 80, 79,136,185, 31, 82, 35, 11, 71, 80,180,174,194,188, 12, 18,
194,128,180,187,188, 38,180,128,136, 80, 32, 79, 44, 46,114, 80, 30, 77, 79,188,
 78, 70,136, 30,188, 79, 32,174, 93,136, 80, 30,188, 79, 46,134, 80,136, 77, 79,
 78, 30, 79,136,152, 80, 32,174, 30,136, 74,188,180, 29,194,127,121,149,188,123,
180,194, 33, 74,163,127, 74,180, 82,131,123,162,188, 80, 82, 78,194, 83,155,170,
 78, 74,123,180, 82,150,170, 19, 33,180,188,194, 46, 18, 30,188, 38,187, 34, 40,
 22,188, 22, 30, 40, 34,194, 70, 38, 30, 80, 78, 79,136, 79, 38, 80, 34,188, 40,
174,106, 30,180, 79,188, 38, 34, 28, 30,188, 34, 38, 40,194, 77, 74,194, 32,178,
 81,130,180, 22,188,180, 79, 82, 81,136, 18,188,180, 81,138, 20, 30, 26,188,180,
 79, 81, 78,136,  4,188,180, 82, 81,136, 14, 73,188,174,180, 80, 84, 81,  4, 30,
148,128, 12, 18,184,  6, 80,114,128,136, 80,180, 36, 22, 66, 82, 80,128,132, 18,
184, 79,174, 32, 22, 80,  0, 28, 16, 83, 82, 80, 12,188,193, 67, 83, 82,128, 80,
184,182, 82, 67, 61, 30, 75, 80,193, 20, 12, 75, 79,136, 68, 89, 80,128, 30, 79,
193,128,178,179, 86, 81, 80, 78, 77,193,136, 10,136, 79, 81, 61, 78, 80, 68,136,
 30, 68, 83,193, 75,136, 66,194, 80,180, 33, 78, 77,157,121, 80,123, 17, 37, 74,
149,123, 78, 74, 17,121, 19,155, 78,121, 17, 74, 82, 79,150, 19, 80, 31,123, 13,
 37,255,255,255,255,255,255,255, 79,128,128,178, 30,179,180,193, 72, 72,188,180,
 82, 81, 86,114, 72,180,136,128,188,118,158,188, 72, 65,128, 82, 81, 10, 30,136,
 65,188,180,174,106, 72, 65, 82, 26, 20, 44, 83, 67, 30, 62, 82,188, 80,193,128,
 30,193,128,178,180,179,114, 62, 22,188,132,136,118, 69, 30, 14,136, 82, 46, 69,
 20, 69, 79, 81,136, 80, 82, 10,136, 79, 81, 62, 78, 80, 80,128, 30,193,180, 79,
128,179, 66, 30, 79,136, 73, 26, 32, 10, 73,174, 30, 79,136, 81,154,108, 87, 32,
136,138,180, 72, 59, 73,188,180, 87, 82, 86,174,136, 59,180, 81, 87, 16,136, 66,
 82, 83,174,194, 37, 78,150, 19,123, 74, 31, 33,193,149, 74,123, 82, 83, 37, 19,
155, 78, 17,121, 74, 82, 33,255,255,255,255,255,255,255,255,255,255,255,255,255,
255, 30, 73,188, 80,180,193, 38,128,155, 24,136, 74,193, 80, 18,114,128, 80,188,
 38,110, 24,148,128, 24,193,188, 36, 80, 66, 83, 82, 80,193,174,180, 22, 80,128,
184,188, 12,136,193, 60,194,132,187,138,110,118, 10, 79,187,136,194, 30, 88,134,
136, 78, 79, 83,190,194, 62,118,136, 83,132,194, 82, 80,194,195,136, 83,187,191,
 73, 83, 82, 30,136,181, 78,192,128,184, 30,193,191,128,255,114,191,193, 78,255,
255,255, 73,136,193,180,174,194,195, 66, 82, 79,189, 83,193,190, 60,194,176,184,
180,136, 22,134,184,191,193,255,255,255, 82,128, 30,193, 79,128,194,255,114, 80,
 79, 78, 81, 68,136, 60, 61, 81,178, 75,136, 83,155, 78, 61, 80,136,193,194, 24,
136, 61, 75,193, 77, 80, 10,136, 79, 81, 61, 78, 80, 83,128, 30,180,193,128,194,
255, 60, 62,188,180,132, 81,174,114, 80,193, 79, 78, 62,136, 73, 62,193, 77,180,
 80, 81, 10,136, 79, 81, 62, 78, 80, 22, 80,136,193, 30, 22, 78, 18, 83,128, 30,
193,128,187,180,179, 10,136, 79, 81, 62, 78, 80,114, 62,136, 22,188,138,128, 66,
 62, 30,194,136, 82,188, 12,136, 79, 80, 81, 78, 77,  2, 62, 30,136, 55, 97, 69,
136,149,123, 79, 83, 82,188, 77,150,121, 19,188, 82, 31,123, 66,194, 82, 83,121,
193,187,157,121, 80, 74,123, 82,188,155,121, 78, 83, 79,188,180,255,255,255,255,
255,255,255, 79,128,128, 30,187,179,194,180,158,118, 65, 32,188, 44,194,114, 72,
136,128,118,132,180, 72, 72,180,188,174, 81, 77,155, 78, 80, 32, 30, 83, 26,160,
 83,136, 78, 80, 65, 44, 82,128, 30, 79,193,128,187,194, 26,136, 79, 80, 78, 83,
 81, 12, 75, 79,136, 68, 89, 80,114, 61,136, 22,110,132,138, 10,136, 79, 81, 61,
 78, 80, 42, 30, 61,136, 83, 75, 68, 30, 30, 83, 82, 12,180, 34, 24,148,128,194,
108,188, 18,187, 79, 32,174,180, 22, 28, 24,158,194, 83, 79,188,118,180, 66, 83,
 82,128,174, 80,184,114,128, 80,180,136, 36, 22,188, 67,136,194, 46, 83, 32, 80,
128,136, 30, 80, 74, 83, 46, 42,136, 79, 80, 78, 30, 83, 30,136, 79, 83, 30, 80,
 32, 24,136, 79, 30, 83, 46,194, 86,174,136,180, 79,167, 88, 74,128, 30,127, 46,
 73,184, 72,129, 81,141,183, 72, 75, 73, 71, 46,121,129, 24, 38,125, 72, 22, 66,
 24,  6, 12, 42, 14,123, 26,129, 60, 75, 67, 81, 24,123,123, 38, 80, 73,129,125,
180,154, 26, 32,188,143, 95, 71, 69,159, 20, 26,178,125,152, 67,129, 26, 32, 67,
 46,143, 64,129, 26,174, 32, 44, 20,  2,129, 26, 32,143, 44, 20, 63,129,174, 26,
 46, 44, 32,178, 66, 73,184,179,255,255,255,113, 72,176,190,180, 46, 22,143,180,
 72, 14,176, 30, 22, 18,194, 72, 30, 73,180,184,115, 72,180,176, 67, 71, 73,123,
194,184, 72,190, 26, 20,193, 79,194, 30,190,192,195,189,113, 72,190, 75, 22, 67,
189,123,189,190,123,192, 20, 32,115, 72,195, 75, 81,189, 71, 66, 72,129, 71,194,
 76, 73, 16, 60,125,129, 88, 81, 53,194,115, 72, 32,195, 60,190, 67,143, 72, 60,
 30, 53, 67, 95,113, 72, 88, 95, 67, 14, 22, 72, 72, 46, 73,188, 75,178, 69, 32,
 26, 76, 72,191, 88, 79, 46, 32, 73, 81, 76, 67,179, 79, 30,129,171,255,255,255,
162,129,180, 67, 44, 53,143,143,180,143,178,171,255,255,115,180,171,178,255,255,
255, 18,180, 72,129,193, 30, 67,  4,180, 60,129,193, 72, 44, 67,194,128, 32, 26,
 76, 20, 72, 75,106, 32, 26, 71, 22, 75, 72, 16, 26, 75, 32, 88, 22, 46, 65, 72,
 32, 46, 22, 95, 14, 66, 73, 75, 81, 76, 71, 46,  4, 32, 26, 44, 22, 73, 81, 32,
 16,194, 75, 73,184, 67,185, 65, 81, 72,194, 38,174,180, 66, 24,174, 72, 81, 38,
129,106, 72, 73, 71, 24,194, 81,148, 71,194, 75, 26, 81,193,128,194,193,180,185,
178,255,129,128,180,193,179, 30,194,187, 60, 26,116,120, 67,138,180, 28, 26,180,
116,174, 44,120,184,144,120,179,142, 81,187, 22, 26,116,180,174, 22, 44, 74,180,
194, 32,187,192,255, 81,148,194, 32,123, 46, 30, 74, 16,123, 44,125, 46, 30,143,
188, 74, 46, 30,192, 22, 14, 65,123, 74,125, 77, 78, 80,114, 74, 32,125, 30, 22,
 44, 22, 30, 22,194,123,125, 46,143, 16, 22, 26, 30, 44,134,128,  4, 26,128,138,
 46,180, 30,106,138,128, 26, 20, 14, 46, 69, 75, 46, 76, 72, 81, 73, 74, 32,180,
130,187,194,178, 18, 75, 71, 73, 72,128,138, 20,128,194,193,178,179,180,185, 16,
 32,123, 81,193,125,129, 74, 32,180,194,185,178,192, 65,174, 38,194, 32, 72,143,
 64,174, 38,194, 44, 72,143,114,125, 32, 38, 44,129,194, 79,129,128,180, 30,179,
193,194,187, 28,116,120, 72, 73, 60,174, 22,116, 60, 72,174, 71,188, 18, 72,174,
120, 26, 46, 44,155, 32, 71,188,174, 72, 26, 72, 30, 73,180,188,174,120, 72,162,
123,125,143, 46, 26, 32, 72,194, 86,143, 80, 78, 82,114,123, 32, 79,125, 71,129,
 16,129,123, 75, 26, 20, 73,155,129, 71, 32, 79, 75,194,164,123, 75,125, 76,143,
 71, 32, 16,174, 81,194, 60, 73,180,114, 72, 67, 81, 24, 20,194, 72,180, 81,174,
194, 38, 72,106, 72, 81,180, 71, 76, 20, 65, 81, 72,194, 38,174,180, 18,180, 72,
194, 81,174, 38, 81,128, 79, 30,194,193,178,255,114,125,129, 80, 74, 82, 60,106,
129,125, 80, 82,123,174,134,123,125, 80,129, 74,174, 65,123, 74,125, 77, 78, 80,
 72, 74,123, 80, 30, 82, 14,143, 83, 46, 75, 76, 72, 71, 22,114,128, 44, 20,138,
 46, 71, 72, 73,138,180, 30, 46, 44, 78, 30, 76, 20, 72, 81, 46, 65,138, 22, 67,
128, 14, 20, 77,128, 81, 76, 72, 22, 46,194,114, 46, 32, 73, 81, 76, 67,106, 72,
 46, 75, 73, 71, 26, 72, 32, 46, 72, 22, 14, 30, 81, 73, 81, 22, 14, 71, 32,134,
 46, 75, 71, 73, 26, 88,128, 46,188, 44, 73, 71, 72, 66, 72, 16, 74,123, 75, 76,
125,180, 65,129, 30,123,194, 32, 74,114,123,129,192, 32,125, 22,150,123, 75, 74,
129, 76, 71,162,123,174, 30, 46, 20,125,148,123,129, 71, 76, 74, 79, 75,128,194,
193,179,180, 30,255, 73,125,174,194, 46,123,129, 67,123, 32, 74,194,178,192, 68,
123, 32,194,125, 74, 26, 59,193,129,125,194,123, 74,114,129,125,194,192, 32, 46,
174,114,129, 32, 22,192, 72, 67, 73,123, 26, 20, 46, 32,188, 16, 67,129, 26, 46,
 88, 81, 67, 72, 32,123, 26, 46, 81, 59,195, 20, 32, 71, 72, 73,  4,123, 72, 67,
 88, 26, 32, 71,128,193,194,179,180, 30,255, 59,193,129,123,125, 74, 76, 69, 76,
 72, 78,194, 46,143, 65,129,123, 74,194,125, 30, 34,174,123, 75,194, 74, 46, 73,
123, 72, 46,143, 85, 73,194,128,125,129, 72, 75, 76, 46,114, 75, 81,190, 95, 72,
 88,106, 71, 76, 73,129, 67, 75, 59, 26,129,189,193, 60,123, 73, 73, 46, 81, 88,
 67, 75, 67, 32, 26, 44, 73, 22, 46, 81,128, 79, 30,194,180,193,255, 59,193,125,
129, 74,123, 80, 18, 67,123,129, 80,125, 30, 65,123, 74,125, 77, 78, 80, 73,123,
194, 80, 46, 77,143, 69,125,123, 30, 82, 14, 46, 93, 46,114,194, 72, 30,129, 38,
 22,128, 30,194,180,179,185,178, 72,194,174,129, 38, 73, 75, 86,194, 38,180,179,
178,192, 16,174,129,194,123, 30,180, 22,180, 22,194, 60, 72,143,180, 65, 26,129,
 32,143,123,174, 97,159,166,152,123, 26,178,106, 32,152,129,125,166, 26,114,129,
 32,125,159, 20,143, 95, 32, 95, 22,188, 20,129,128,159,152, 32,129,166, 20,143,
114,128, 67, 72, 71, 30, 20, 72, 73,138,180, 30, 46, 44, 65,138, 22, 67,128, 14,
 20, 79, 72, 81, 44, 20, 75, 73, 92, 72, 75,174, 76, 30, 14, 86,174, 75, 72, 71,
180, 76, 32, 72,180, 81,174,194, 38, 72,114, 72, 81,180, 24, 67, 71, 65, 81, 72,
194, 38,174,180,106,174,180, 38, 81, 72, 71,  4,180, 81,174, 60, 72, 24,128,180,
178,185,255,255,255,123,128, 30,180,128,178,179,255, 86,174, 82,136,180, 75, 61,
 72,136, 61,180,138, 75, 96,114,136, 54,138, 68,110, 30, 65,136, 61,138, 68, 90,
 54,155, 32, 68, 90,138,180, 61, 72, 72,194,143, 86,125, 22, 14,162,158,123,125,
143, 93, 46, 16, 75,129,123,125, 76, 30,164,143,123,125, 75, 76, 74,114,129,123,
 32,125, 93, 22,148,123, 71,143,174,158,129, 86,174, 93, 32,143, 22,188,160, 95,
 72, 73,129, 32, 30, 26, 46, 65, 26, 20,129, 22, 67, 32,114,129, 22,160, 30, 73,
123,134,129,160,153, 67, 73, 26, 79,129, 26, 75, 72, 73, 32,180, 65, 26,129, 32,
143,123,174,114,129,152,125,159,192,166, 16, 32,152,173,181,129,123, 93, 32,188,
143,166,129,152, 22,129,174,188,166,123,173, 18,166,152,174,188, 32, 26,194,128,
 95,188, 46, 81, 60, 53,134,190, 73, 75, 71,188,189,106, 46,195, 14, 22, 75,188,
 72, 32, 46, 72, 22, 14, 30, 85, 95,188, 44, 73, 32, 71, 93,188, 46, 95, 44,143,
255,143, 72, 73,138,180, 30, 46, 44, 65,138, 22, 67,128, 14, 20, 79, 72, 81, 44,
 20, 75, 73, 85, 72, 75,174,128, 76, 73, 93,174, 75, 72,180, 73, 46, 84,174, 76,
 73, 75, 71,180, 72,164, 86,123,125, 74, 75, 71, 72,143, 44, 87,129,125, 14, 16,
 74, 75,125,123,143, 76,162,174,143,123,125, 86,158,114,129,125,123, 86,192, 79,
155,129, 75, 71, 74,194, 86, 81,128,194, 79,193, 30,178,255, 65,123, 74,125, 77,
 78, 80,106, 80,123,125, 77,129, 79, 72, 74,123, 80, 30, 82, 14, 79,123,125, 80,
143,194, 46,114, 79, 77,125, 74,129, 80,125, 79,116, 18,123,117,121,113,103, 80,
148,121, 67,131,129, 82,125,155, 32,121,174,188, 67,187,164,121,101,129,125,131,
111,163, 67, 80,174,101,188,129,149,121, 83,131,188, 67, 80, 46,114,194, 38, 82,
 80, 53, 74, 86,194, 38,178,192,255,255,  2,194,187,138, 82, 60, 38, 65, 67,194,
 38, 82, 30, 83, 26,194, 60, 38,187,138, 40, 78,194, 67, 82, 38,130,187, 80,154,
130,116,138, 87,180,110,114,116, 81, 79, 73,194, 26,148, 73, 81,116,130,138,110,
149,116,188,180, 73,130, 81,162, 87,130,138,116, 32, 44,155,116,110,188,180,130,
 32, 83,128, 30,194,193,187,178,179,148,130, 80,116, 81,138,110,155,116,130,110,
 80, 82, 32,  2, 80,116, 81, 82, 46,187,163, 80, 81,138,158, 32,116, 82,116, 82,
188, 32,130,138, 82,  2, 80, 46, 61, 81, 83,187, 26, 81,116, 80,187, 26, 83, 18,
 81, 80,187,194,188, 46, 65, 80, 81, 75, 32, 68, 78,114,116, 54, 80,194, 81, 79,
 72,116, 54, 68, 75, 78, 32,138,128, 79, 30,194,193,178,255,114,125,129, 80, 74,
 82, 60,106,129,125, 80, 82,123,174,134,123,125, 80,129, 74,174, 65,123, 74,125,
 77, 78, 80, 72, 74,123, 80, 30, 82, 14, 68,116, 18,123,117,113,121,103, 22, 67,
 32, 30, 14,187, 46, 74,164,174,131,121,111, 22,129,148, 78,121,174,129, 32, 74,
155,174, 32,121, 14, 44, 67,149,174, 14,121,131,162, 22, 67,128,178,193, 30,179,
255,255,148,178,192, 26, 64, 74, 53,  2, 32, 26,130, 20, 22, 81, 28, 32, 26, 22,
130, 20, 74, 16, 32, 26, 81,116,130, 30,106,116,154, 32, 26,130, 63, 30, 65,116,
 38,110, 14, 46, 83, 75, 24,116,184, 22,  6,185, 64,194, 67, 82, 38,116, 36, 34,
184, 60, 82,178,116, 46, 18, 18,184, 82, 80, 22, 83, 26, 18,116, 60,138,192, 46,
 82,148, 78, 75, 46, 26, 81, 79, 67,194, 32, 81,187,116,130,128, 79, 30,194,187,
178,193, 65, 80, 81, 75, 32, 68, 78, 18,188, 26, 30, 75,116, 32, 75, 77, 80, 46,
 83, 89,116, 80,154, 87, 32,116,110, 26, 44,162, 87, 32,116,110, 26, 44, 64, 79,
 81, 82, 22, 44,110, 69,194, 73, 77, 81, 78, 83, 18, 82, 30, 78, 77, 26, 32, 66,
194, 82, 77, 81, 14, 20, 83,128, 30,178,193,179,255,255, 82,116, 82,188, 32,130,
138, 64, 79, 81, 82, 22,187,138,155, 78,188,116, 32, 82, 26, 69,194, 81, 77, 80,
 78,116, 66,188, 82,194, 77, 80, 30, 66,188,106,130, 30,194,195, 46, 67,128,195,
 53, 88, 67,110, 60, 59,194,187,195, 82,181, 74, 16, 30, 67, 78,194,195, 60,114,
 77, 78, 79, 67, 44,195, 26, 79,180, 44, 30, 77, 78, 82, 67,194, 32, 81,187,116,
130, 65, 80, 81, 75, 32, 68, 78, 64,116,194, 79, 30, 81, 46, 26,194, 80, 81, 46,
 26, 89,  4,194,188, 80,110, 61,187, 34,194,188, 61,116, 46,130,116, 18,123,121,
117, 22,103,113, 67,120,124, 32,130,110,102,148, 78,194,131,121, 46,129,155,194,
 14, 79, 80, 32,121,149, 79,194, 14, 80,131,174,163,101, 22, 79, 80,174, 46,194,
 68,138, 82,195,193,255,255,106,116, 79, 77,195, 82,188,114,116, 77, 88, 67, 79,
 46,128,116, 46, 80, 67, 74, 82, 65, 46, 78, 95, 77,159, 32, 73,116, 79, 32, 46,
138, 95, 67,128,194,187,180,193,179, 30, 67,116,180,130,178,194, 32,184,188,130,
193,187,110,116,148, 53,194,188,180, 46, 14, 65, 46,187,130,116, 81, 68, 63,116,
194,187, 46,179, 64,180, 59,187,181, 74,173,255,255,106,194, 79,188,130,187,116,
 16, 30, 46,194, 67, 60,181,114,194,110, 60, 79,192,187,128, 79,116,110,130, 60,
188,134, 79,194, 67,159, 80,187, 64, 79,148, 78, 72, 65,130, 81, 80, 16, 80,116,
 86, 93, 72, 77, 65, 80, 78, 81, 86, 82, 77,155, 72,188, 81, 78,110,180,128, 30,
179,187,193,194,178,114,116, 72, 44, 20, 32,138, 74,128, 30,180,179,193,194,187,
 92, 72,116, 81,180, 76,188, 78,116, 30, 81, 46, 72, 76, 16,116, 75,188, 73, 67,
 72, 71,116, 72,188, 73, 46, 20, 68, 67,116,188, 75,138, 81, 82,128, 79, 30,194,
187,193,179,148, 78,194,116, 46, 75, 44, 67,194, 32, 81,187,116,130, 65, 80, 81,
 75, 32, 68, 78, 18, 79,116,187, 61, 80,188,155,110,130, 75, 78,116, 81,187,114,
 44, 46, 22,194, 82,152, 68, 82,195,193,255,255,255,128,152, 67, 79, 82,116,194,
 66, 79,116,188,194, 67,193,154, 60,194, 77, 74, 32, 88, 65, 67, 79, 80,159,116,
 32,116, 67,122, 30, 74, 46, 14, 98,148, 67,121,129,187,194,101, 18,123,117,113,
121, 79, 67,164,121,131, 82,125, 22,129,155,101, 32,121, 67, 78,194,149,162, 83,
188, 67, 74, 22, 44, 65, 38,116, 36, 28, 77, 78,128, 79,194,193,178,185,187, 71,
 38, 82, 83, 80, 28, 36, 68, 38, 36, 28, 77, 80,116, 63,116, 82, 38, 36, 80, 77,
 66,188, 36, 38, 82, 28, 77, 69, 67,128,178,194,193,179,187, 30,148,178,192, 64,
 32, 26,194,  2, 32, 26, 22, 46,194, 60, 22, 81, 26, 32, 60, 74,193,106, 32,178,
 68,192,154, 26, 16, 30, 81, 26, 32,194, 22,194, 68,138, 82,195,193,255,255, 18,
116, 79, 82,190, 30, 32,128, 67, 82, 80, 46, 88, 32,114, 82, 77, 32, 46, 79,192,
 34,116, 79, 82, 32, 46,138, 65, 46, 78, 95, 77,159, 32, 77,148, 78, 30, 32, 81,
 26,138, 65, 80, 81, 78,116, 82, 30, 66,188,194, 82,180, 80,138, 68, 81, 82, 30,
 26, 83, 22, 63,116,194, 82,187, 81, 78, 16, 81, 82,192, 22, 80, 26, 80,154,194,
 87, 32, 26, 44, 30,148, 78, 26, 81, 32, 73,138, 64, 79, 81, 82, 22, 44,110,162,
 30,194, 87, 32,138, 46,106, 87, 73,192, 32, 79, 77, 66,194, 82, 77, 81, 14, 20,
 78, 64,194, 79,116, 81, 71, 30, 65,194, 81, 80, 71, 82, 77, 18, 14, 32, 81, 30,
 71, 26, 22, 81, 71, 82, 32,194, 77, 34,179, 14, 82, 30, 81, 80, 66,180,188, 82,
 77, 30, 80, 30, 65,116, 38,110, 14, 46, 83, 64,194, 67, 82, 38,116, 36, 26,184,
 60,116, 18,194,182, 18, 82,184, 22, 24, 42, 46, 66,188,184,180, 82,116, 36,106,
 24, 22,192,  6, 79,184,106,116, 67,123,127, 22,109, 32,129, 18,123,117,113, 80,
 32,103,148,121,131, 74, 79,129, 67,164,121,125,131,111,101,129,149,121, 67, 30,
 74, 77, 79,255,255,255,255,255,255,255, 30,121,116,130, 79, 74,184, 80,115,116,
180, 79,187, 67,110,119,116, 38,110, 14, 46, 83, 63, 38, 36, 79,184, 82,116, 72,
116, 80, 79, 74,130, 60, 68,116,184, 24, 79,154, 82,130, 69, 32,117,125, 75, 46,
 14,119,121, 72, 75,125, 73,117, 16,117,125,135, 69, 32,193, 67, 72,125,192, 75,
 71, 73, 68, 72, 32,125, 73, 14, 22,115,121, 72,180,117, 71, 75, 74, 72,116, 30,
 73, 67, 60, 53,121,116, 72,138, 73, 30, 26, 66,116, 20,130, 75,192, 32, 68, 26,
116, 22, 75,188,138, 64, 72,116, 71,110, 67, 70, 18, 76, 72, 67, 73,188, 46, 67,
121,130,193, 74,187,194,138,119, 46,187,130,116, 81, 68, 66, 74,130,193,116, 81,
192,101,130,110, 74, 81, 53,138,148,130, 74,138,110, 30, 81,  2,138, 74,130,179,
 60, 20, 46,121, 30, 79, 22, 80, 14, 34, 72, 74, 79, 80, 67, 53, 77, 18, 67,130,
194, 79,138,154, 66, 38,188,194,116,130, 74,115, 77,180,116, 53, 30, 79,  2,130,
 38, 60, 30,138, 82, 77, 26, 81, 18,123,129,125, 83, 74, 30, 10,123, 74,125, 77,
 78, 80,  2,123, 74, 30, 80,129, 46, 72, 60, 74, 30,123,174,194, 34,123,129, 60,
 74,174,194,114, 60,129,125, 67, 53, 46,174, 32,123, 81, 32, 79,181,195,114, 22,
 32, 26,166,129, 30, 10,123,129, 81, 80, 83, 70, 72, 81, 80, 79,166, 82,129, 18,
123,129, 30, 70, 80, 81, 38,129, 81, 80, 79, 84, 78,123,114,136, 22, 70,128, 26,
138, 72,188,180, 79, 81, 78,136, 67, 81, 83, 82, 80, 46, 30, 69, 80, 81, 83,136,
 78, 30, 10, 79,136, 81, 30, 78,188,158, 70, 81, 79, 80, 83,180,194, 72, 83, 81,
129, 32, 79, 30, 38, 79, 91,143,192,178, 81,114, 22, 26, 83, 32, 79, 14, 10,129,
 79, 46, 78, 80, 14, 93, 46,143, 44,255,255,255,106, 79, 30, 83, 32, 46, 22,129,
 69,194, 46, 22,116, 32, 44, 10, 84,194, 20,116,120, 22, 79,174, 78,151, 26,194,
116, 86,174,151,116,194,120, 80, 20, 78,120, 20,116, 30, 84, 18, 78,194,174, 26,
 84,120, 30,188, 38,182,184, 84, 46, 91, 72,184, 78,129, 38, 46,178, 67,194,123,
178,192, 18,255,160, 70, 79, 46,123,129, 38, 66,174, 24,180, 46,194,172, 18,123,
125,129, 78, 38, 84, 18,123, 67, 83, 81, 80, 82, 30, 20, 72,188,180, 81,138, 20,
 30, 10, 79,136, 81, 30, 78,188,158,118, 70, 83, 81, 80,188, 24,136, 83,174, 82,
 30, 80, 30,136, 79, 30, 46, 78, 20,129, 24,120, 78, 84, 91,116,138, 10, 84,194,
 20,116,120, 22,159,114, 70,138,124,120, 91, 63, 78,194, 22, 26, 32, 46, 42, 78,
174,194, 26, 46, 22,  6, 78, 26,174,194, 46,124, 83, 10, 81,123,125, 77,129, 78,
 12, 81,129,194, 62, 77,125, 67,123, 32, 81,194,192,178,158,123,194, 30,180, 81,
 69, 64,123, 79, 62,194, 80,174,128, 30,194,193,179,178,255, 78,128, 30,194, 79,
193,179,178,158, 57, 81, 92,180, 71, 83, 24, 81, 83,129,125,123, 77, 67,123, 32,
 81,192,194,178,188, 83, 30, 77, 32, 57, 81, 64,129,123, 79, 80, 32,174, 80,128,
 79, 30,194,193,179,255, 24, 81,123,194, 83,125,129, 66,123, 73, 32, 26, 20, 87,
 67,194,123, 81, 32,192,255,154,194,174, 87,180,123, 32, 12,123, 81,129,194, 87,
 77, 81,128, 79, 30,194,193,178,255, 67,123, 44,125, 46, 30,143, 66, 67,123,129,
 80,125, 30, 10,123, 74,125, 77, 78, 80,158,129,123,125,194, 46,180,  2,123, 74,
 30, 80,129, 46, 16,123, 61,188, 82,136, 26, 81,193,114, 80, 79,136, 78, 20, 81,
 62, 83, 20,136,180, 91, 84, 10, 79,136, 81, 30, 78,188, 66, 83, 79,136, 80, 82,
 81, 22, 80,136,193,180, 22, 20,125, 60,192, 81,194,130, 32,178, 73, 80,116, 26,
 32, 14, 84,148,116, 70, 78, 30, 32, 14, 22,116, 22, 80, 44, 30, 26, 10, 80, 81,
 78,116, 82, 30, 24,193,194, 82, 26, 30,116, 26, 73,129,125,123, 38, 20, 34,150,
129, 38, 80,180,125, 81,128, 79,193,194,255,255,255,114,125,129, 80, 18, 34, 81,
 10,123, 38, 44, 84, 32, 80,148, 81, 80,129, 78,123, 32, 32, 60, 81,184,194,192,
143,125, 10, 38,194,129, 44,125, 78,114, 80, 79, 20, 26,129, 44, 73,129,125, 38,
 20, 81, 26, 66, 24,129,125, 20, 38,184, 61,194,129,125, 80, 78, 24, 44,114, 79,
129, 32, 26, 38, 36, 60, 81, 32,192,194,178,185, 73,129,125, 26, 20, 38, 81, 10,
123, 38,129, 81,143,125, 61, 32, 26, 81,125,143,184,150,129, 38,143,123, 81, 80,
 14, 60,194, 81,192,178,185,123,188,184, 30, 46, 78, 38, 80, 73,125,129,123, 38,
 22, 78,128, 79, 30,193,194,255,255,114, 80, 38, 22,125, 79, 78,150, 83, 38,123,
125, 80,143, 67,194, 65,129, 79, 46, 78, 80, 14, 64,129, 79,143, 44,123, 46,106,
129, 32, 84, 49, 79, 26, 63, 32, 79,129, 30,125, 26, 81,125, 80, 44, 46,129, 84,
 16, 80, 26, 14, 81, 22, 44,123, 16, 83, 81, 80, 82, 30, 20, 69, 30, 83,136, 81,
 80, 46, 60,136, 20,188, 80,180, 78,  4, 82, 80, 83,193,188, 81, 66,174, 81,136,
 79, 83,193,  2,174, 82, 30, 81, 80, 83, 81,148,194, 32,123, 46, 30, 74, 16,123,
 44,125, 46, 30,143,188, 74, 46, 30,192, 22, 14, 65,123, 74,125, 77, 78, 80,114,
 74, 32,125, 30, 22, 44, 22, 30, 22,194,123,125, 46, 32, 16,184,143, 40,125, 78,
 81, 65, 38,194,129, 44,125, 78, 64,174,123,125,129,143, 38, 22,194,125, 81,129,
 78,143, 63,194,174, 24, 84, 38, 44, 18,129, 24, 80,174,  0, 78,178, 18,179,255,
255,255,255,255,134,171,255,255,255,255,255, 65,194, 30,160, 79,123,143, 66,172,
179,255,255,255,255, 16,184,176, 22, 46, 20, 26,114,184,177,170,171,255,255,192,
134,250,201, 54,212,255,255, 65,194, 79, 30,174,193, 44,162,129, 80, 78, 32,125,
 46, 81,194,174, 70, 30,190,195, 53,194, 70, 79,176,195,174, 16, 70,125,129,184,
 91, 32,150,123,158, 81, 83,188, 82, 70, 79, 67, 22, 32,194, 30, 83,180,166, 83,
 70,132, 82, 80, 91,151, 83, 79, 70, 81, 78, 80, 68,174, 26, 83, 82,110, 20,152,
 79, 81, 82,174, 70,108, 81, 67,123, 30,194,125, 46, 22, 18,123, 30, 32,129,125,
 83,148,123, 74,129, 79, 46, 78,166,129,125,123, 74, 80,143,153, 83, 74,125, 53,
123,129,152,123,125,129, 74, 79,194,125, 67, 32, 81,180,130,116,192, 18,188, 78,
 80, 81, 83,110,148, 78, 30, 32, 81, 14,116, 16,116, 81, 32, 83,138, 78,166,130,
 81,188, 70,138, 30,152, 81, 70, 82,116,188,138, 83, 69,123, 30, 81, 69,194, 82,
  4,123, 80, 81, 46, 30, 82,147,123, 76, 77, 81,194, 79, 67,123,194,180, 81, 32,
178,148,123, 81, 79, 76,194, 78,152,123, 79, 76, 81,129,194, 82, 67,194, 81,123,
 32,192,178,147,123, 75, 81, 77,143, 68, 26, 81,123, 80, 46, 30,129,  4,123, 80,
 81, 46, 30,129,153, 83,129, 75,143, 14, 81,148,123, 68, 81, 79, 75, 80, 78, 67,
194,123, 32, 81,180,178,128, 30, 79,194,180,178,193, 66,123,194, 83, 82, 71, 50,
 63, 50,123, 83, 71, 64, 82, 69,194, 81, 82, 83, 30, 80,147, 71,123, 77, 81,194,
 64, 69,194, 65,129, 79, 46, 78, 80, 14, 64,129, 79,143, 44,123, 46, 63, 32, 79,
129, 30,125, 26, 67, 79,129, 78, 26, 80, 84, 83, 44, 32, 46, 78, 30, 80, 76, 26,
 32, 83, 46, 80, 20,125,148, 78, 30, 32, 81, 26,138, 65, 80, 81, 78,116, 82, 30,
 66,188,194, 82,180, 80,138, 68, 81, 82, 30, 26, 83, 22, 63,116,194, 82,187, 81,
 78, 16, 81, 82,192, 22, 80, 26,123, 16,136, 80, 83, 30, 78, 81, 90,180,138,193,
178,194,179, 67,174, 30, 82,188, 80, 83, 66,174, 81,136, 79, 83,193, 65, 79,136,
 81, 30, 78,188, 26, 80, 81, 83,136, 78, 30,129, 16,116, 32,120, 46,138,124,  4,
116, 26, 14, 46, 22,120,150, 46,138, 32, 22, 44, 78, 26,194, 46, 22,116, 32, 44,
  2,194,120, 46, 22, 44, 78, 22,116, 26, 46, 78, 30, 44, 30, 16,125, 24,123,  6,
 36, 42, 22,125, 24,123,129, 78,194, 97,123,129, 78, 24,184, 14,150, 46, 78, 14,
125, 42, 36, 68,123,125, 12, 24, 38, 18, 90, 24, 18, 14,  6, 42, 38, 82, 76, 46,
 89, 30, 78, 20, 14,128, 30, 79,178,194,179,193, 97, 30,123, 89, 46,125, 14, 55,
 30, 89,123,125, 14, 46, 66,194, 75,125, 89, 81, 77, 62, 89,123,125, 20, 30, 81,
 78,128, 30,127, 73, 38, 86, 94,129, 24, 16,129,123, 77,125, 71, 79,121, 79,129,
 24,  6, 12, 71,131,129, 79,184,183, 71,125, 26,129, 79,125, 24, 71,123, 66,123,
 24, 79,174, 77, 42,194,113, 79,125,143,129, 20, 80,  2, 79, 46,129,178, 77, 44,
143, 79,193, 77,143, 80, 46, 26,129, 77, 46,125, 79, 71,123, 79,129,143,190, 46,
 82, 79, 46, 79,129,190,143, 77, 79, 16, 80,193,123,174, 20, 44,122,192,194,178,
193,179,255, 18,123,174, 32,125, 72,129,155, 32,174, 80,129, 20, 81,  2,129,174,
123,125,194, 46,150,123,125, 30,129, 72, 32,178,168,184, 79,157, 71, 30, 77,113,
125, 79,123, 22, 77, 20,123,180, 77, 32, 14, 20, 79, 18,194,184, 30, 79,166, 32,
119,194, 30,143,160, 79, 14,143, 79, 30,160,194,190, 46,193,123, 79,190,143,179,
189, 77, 69, 79, 30,123,191,158, 26,  2, 71, 32, 79, 77,191,194, 18,194, 79,129,
123, 77,191,119,194, 79, 80, 77,143, 14, 26,194,123,129, 71, 79, 32,179,143,178,
143,171,255,255,255, 16,173,255,255,255,255,255,188,123,193, 30,129, 64, 71,168,
 79, 85, 92, 50, 57, 71, 67,178,123,173,171,255,255,  2,123,193, 79, 30,178, 71,
188, 83, 34, 61, 62, 73, 32, 80,174,181,123, 32,143,129, 44, 46,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255, 81, 67, 74, 46, 30,192, 22, 14,128, 30, 79,178,193,255,255,148,
123, 74, 79, 46, 80, 78, 79, 80, 46, 74, 83, 60, 53, 66, 74, 32, 46, 53, 60, 30,
  2, 74, 32, 67, 83, 79, 53, 46, 67, 81,192,178,185,255,255,128, 30, 79,178,185,
179,255,164, 64, 79, 83, 80, 85, 77,160,194, 77, 81, 82, 83, 57,153,194, 81, 71,
 40, 82, 79,167,123,194, 82, 79, 80, 40, 32,128,193, 79,178,185,255,255,160, 80,
 81, 71,125, 77, 79, 86,192, 38,178,193,255,255,153, 24, 26, 38, 71,143,129,164,
 81, 40, 80, 79, 64, 85, 16, 40, 80, 77, 81, 24, 16, 22,128, 30, 79,193,179,185,
178, 67, 81,185,192,178,255,255, 18, 16, 30, 38, 46, 82, 14, 68,184, 82, 38,182,
 46,192, 86,192, 38,178,179,193,255, 16,184, 80, 81, 14, 46, 77, 30,128, 79,193,
178,185,179,255, 18,182, 38, 71, 22, 77, 18, 16, 24, 18,184, 12, 77, 42, 26, 38,
 85,182, 46, 77, 92, 67,185,192, 18,178,255,255, 66,184, 24, 18, 12, 14, 22,150,
 50, 51,125,129, 71,123,194,193,  2,123,125, 71, 85, 64, 78,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,123,158, 81, 83, 71, 57,180, 82, 67,180, 50,188, 32,194,192,147, 64,
108, 77, 81, 79, 71,166, 64, 83, 50, 82,132, 80,152, 64, 79, 50, 82, 81, 71, 16,
114, 71, 83, 57, 82, 80, 71, 69,123, 74, 75, 30,194, 46, 63,123, 50, 75,129,125,
 74, 66,123,129,125, 75, 74,194, 67,194,180,123, 32, 74,192,  4,123, 50, 73, 30,
129, 32,166, 74, 75, 73,123, 72,194, 81, 67,123, 30,194,125, 46, 22, 18,123, 30,
 32,129,125, 83,148,123, 74,129, 79, 46, 78,166,129,125,123, 74, 80,143,153, 83,
 74,125, 53,123,129,152,123,125,129, 74, 79,194,125, 67,180, 81, 32,130,116,194,
 18, 50, 71, 30, 92,188, 85,147, 71, 77,130, 81,188, 79,166, 81,188, 71, 64,130,
 80,153, 71, 81,180,188, 26, 64,152, 81, 82,116, 80, 64, 50,180,106,129,123,125,
 26, 81, 32, 79, 32,188, 79,129, 30,172, 64, 32,188, 80, 92,129, 85,  2,129, 80,
 32, 26, 44, 20,153,125, 26, 64, 44,129,143,147,188,125, 26,129, 50,174, 16,125,
 60, 81,130, 32,194,178,192,128, 30, 79,194,193,255,255, 10,194, 81, 80, 71, 82,
 77, 66, 82, 81,188,180,194, 30, 73, 80, 77, 71,116, 46, 81,  8, 81,193, 82,116,
 80, 83,129, 60,192,194, 32,116,255,255,128, 30, 79,193,194,255,255, 10,120, 71,
 32, 80, 20, 14, 73, 71,120, 77, 80, 44,116, 24, 26, 30, 20,120,116, 80,  8, 32,
 20,116, 26, 30,120,123,128, 30,180, 79,193,128,194, 60,188,180, 71, 81,174,178,
114, 80, 79, 20, 14,180, 81, 73, 77,174, 80, 81,180,188, 66, 83, 30, 71, 79,136,
 80, 22, 80,136,193,174, 71, 22, 14,188, 81, 77, 22, 46,184, 80,128, 30,193, 79,
194,255,255, 61,125, 81, 82,143, 57,192, 73, 22, 57,143, 77,123,129,114, 57,123,
125,143, 77, 30, 22, 22, 80,123,125, 71,143,143, 60, 32, 81,130,194,178,255,114,
134, 80, 79,138, 14, 30, 73, 83, 14, 71, 82,130, 20, 62, 20, 83, 32,138, 46, 82,
 61, 14, 32,130, 71, 82, 83, 22, 79,128, 22, 80, 30, 20, 22, 60, 81,194,185,178,
192,123,128, 30,193, 79,194,255,255, 73, 71, 80, 77,129,184, 14, 61, 81, 82,125,
143,178,170,150,129,125,123, 16,143, 80,188,184, 80, 81, 14, 46, 77, 68,194, 63,
 30, 79, 32, 26, 20, 22, 64, 30,123, 32, 22, 79, 44, 65, 79, 32, 20,125, 46, 92,
128, 77, 79, 32, 71, 85,178, 66, 79, 82, 83,123, 81, 71, 26, 79, 32, 71, 22, 26,
 77,123, 75, 71,193, 81, 83, 80, 14,128, 79, 30,178,193,128,179, 67, 30, 71, 82,
193, 80, 83, 22,174, 83, 71, 80, 81,136, 16,136,193, 83, 82, 81,188,  4,136, 71,
 83,193, 80, 81, 81, 65,123, 74,125, 77, 78, 80, 66,123,194, 74, 77,125,193, 69,
125,123, 30, 82, 14, 46, 67,194,123,125, 22, 30, 14, 75,194,123, 74, 22, 46, 77,
114,125, 22, 79, 80, 77, 30, 77, 16, 82,125, 30, 26,143, 81, 18,123,194, 81, 32,
125, 26,188, 26, 83, 70,192, 78, 82, 67,194,123, 81, 32,178,192, 66,123,194,125,
 81, 26, 80,150, 83, 81,123,194, 82, 78, 32, 65,194,129, 38,125, 81, 85,128,194,
193, 79,178,185,255, 16, 81, 71,125,129, 24, 40, 64,129, 81,194, 24,174,123, 75,
194, 57, 77,193, 24, 71, 28, 81,174,129,194, 57, 71,125, 18, 30, 82, 32, 46, 44,
 22, 16, 81, 32, 30,116, 14, 71, 64,194, 79,116, 81, 71, 30, 34, 82, 26,116, 14,
 22, 30, 65,194, 81, 80, 71, 82, 77, 26, 14, 30, 82, 32, 46, 77, 72,125, 79,116,
194, 46, 32, 20, 44,128, 79, 30,194,178,187,193,154, 77, 57, 81, 71, 79,138, 71,
 30,116, 20, 46, 80, 82,106, 79, 30, 80, 85, 32, 83, 65,194, 81, 80, 71, 82, 77,
129, 73, 77, 80,192, 46, 44,116,128, 79, 30,194,187,193,179, 79,179,116,194, 46,
187,120, 65,120, 71, 32, 80, 20, 14,106, 71, 80, 92, 79, 85, 77, 74,180,192, 32,
194,187,255, 80,128, 79,193, 30,194,179,255, 28, 81,125, 73, 83, 82, 78,114,123,
125, 79, 87, 73, 77,150, 82, 78, 83,194,125,123, 71,123,194,125, 83,143, 81, 16,
125,129, 73,123, 83, 87, 71, 75,123,125,143, 32, 73, 74,128,178,193,194, 30,179,
255, 74,180, 32,194, 74,192,178, 86,143, 72,174,123,158, 74, 65,129,123, 74,194,
125, 30,  2, 72,123,125, 46,194, 30,123, 74,194,178, 32, 81,130,180,128, 79,180,
178,128, 30,193, 22,188,136,180, 79, 82, 77,106, 79,180, 71, 77, 81,188,  4,188,
180, 82, 81, 71, 57, 73,136, 71, 80, 81, 77, 30, 82, 74, 32, 81,194,178,192,255,
128, 79,178,194, 30,193,179, 75,123,125, 77, 80, 14, 89,134, 77,123, 80, 79, 89,
125, 79,123,125, 80, 81,143, 83, 76, 46, 89, 30, 78, 20, 14, 80, 72,125,106,116,
 30, 73, 79,130, 83,148,138, 78, 73, 82, 77,158, 71,116, 73, 78, 81, 82, 83, 79,
116, 79,158,194, 81, 83,128, 79,194, 30,193,187,179, 74, 32, 81,130,180,194,187,
 73,128,194,193,178, 30,179,255, 79,123, 72, 80, 46,143,174, 86,174, 80, 72,143,
158,123, 34,123,125,129,174, 87,143,  2,123,125, 75,174, 74,143, 28,123,125,129,
 87, 30,143,129, 74,180, 32,194,192,255,255,128, 30,193,194,179,255,255, 75,194,
 44, 32,116,120,142, 79,120,116,194, 46,138,174,162, 68,194, 46, 54,138, 44,155,
138, 32, 61,194,124, 54, 30, 75,194, 24,123,184, 46, 38,114,123, 38,125, 24, 79,
 73, 71,123,125, 38, 83, 46, 24,154, 79,194, 24, 38, 18,184,149,123, 82, 73, 14,
125,143, 16,123, 59,125, 73, 83,143, 78, 22,125,123,129, 80, 30, 22,128, 79,193,
 30,178,194,179, 26,129, 81,123, 83, 71, 26, 71,123,125,129, 80, 30, 83,106,129,
 79, 92, 85,125, 77,154, 32,194, 77, 71, 79, 44,123, 74, 81,130,180,194,192,187,
128, 79,180, 30,128,193,179, 28, 59, 73,188,180,136, 87, 71, 73,180, 30,188,174,
136, 79,136,174,188, 81, 87, 73, 65, 73,174, 30, 79,136, 81,128, 30,121,123, 79,
129, 73,125, 38,127,123, 79, 73,184,125,129,131,123, 79, 81,184,125,129, 26, 79,
125, 81,123, 24, 73,141, 79,193,194,158,123, 73,123,123, 79, 81,129, 73, 38, 79,
124,194,192, 26,193,179,255,114,125, 77,123, 80, 20, 72,116,129, 32,123, 20, 44,
125,142,193,194,192, 44,179,255, 16,129,194, 30,123,125, 72,134,129,123, 32, 44,
 81, 80,194,123, 79,125,190,143,189, 82,113, 87, 79,125, 22,143,129,115,188, 46,
 32, 79, 30, 77, 64, 79, 46, 44,125, 87, 73,148,129, 79, 87, 81, 83, 46,143, 32,
 79,193, 14, 77, 30,193, 64, 44,143,194,125, 30,158, 63, 44,194, 30,143,158, 26,
 26, 87,194, 83,125, 81,123,123, 79,190,189,179, 73, 77, 18, 87,194,123,129,125,
 73, 69,191,125, 30,158, 83,189,179,143,143,171,255,255,255,255, 67,123,173,171,
255,255,255, 18, 87,123,167, 44,193, 73, 79,167,193, 44, 87, 46,125, 66,193, 73,
 79,125,180, 46,  2,123, 87, 46,193, 73, 79,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255, 16,194,106,129,125, 73,
 26, 20, 44, 61, 81,129,190,192, 73,191, 10, 79, 87,129, 30, 46, 73, 73,129, 87,
 26, 83, 81, 46, 60,129, 26, 87,125, 32, 73, 62,129,125, 26, 73, 83, 87, 87,128,
194,193,255,255,255,255, 73,194, 15,123, 94, 84, 23, 66,194, 25, 86,129,123,180,
 10, 86, 94,194, 25, 43, 85,148,194, 25, 80,129, 37, 86,150, 19,194, 15, 80,193,
129, 26,128,194,193,185, 79,255,255, 73,194, 73,123, 87,143, 77, 66,125,129,143,
123, 18, 38,148,194, 73, 38, 81, 32, 20, 24,194,129,125, 18, 32, 38,154, 59, 38,
180,123, 32,129,129, 73,194, 32, 46, 61, 22, 82,128, 30,194,193,255,255,255,154,
194, 46,138, 75, 26, 68,155,194, 32,138, 26,124,193,162, 26,194,138, 46, 32, 44,
 61, 68,194,116, 75,120, 26,123,128, 30,193, 79,128,180,194, 24, 87, 82,193, 20,
 77, 83, 10, 73,174, 30, 79,136, 81, 66, 83, 30, 73, 79,193,194, 52, 83, 82, 30,
 73,174,136, 73, 73, 30,174, 77,188, 81,125, 60,194, 32, 81,130,192,116,128, 30,
194,193, 79,255,255,148,116,130,138, 26, 59, 78,  8,116, 32, 26, 81, 87,193, 10,
 81, 78, 73, 82, 77,116, 66, 82, 81, 32,194, 30,188, 66, 87,128,194,193,180,179,
255,255, 16,194, 25, 86,129,123,180, 65, 86, 94,194, 25, 43, 85,149,194, 86, 80,
 84,174,129, 73,123,194, 25, 86,129, 94, 67,194,123,192, 25,255,255,123, 69,180,
 83, 81, 77,136, 78,114,136, 77, 73, 87, 26, 32,128, 30, 79,193,194,128,187, 18,
194,174, 73, 30, 82, 87, 59,136,193,179,194,187, 77, 26, 73, 87, 30,194, 79,136,
194,106,129, 87, 73, 77, 20,125,134, 87, 77,129, 73, 79,125, 64,129, 79,143, 44,
 46,125,114,129, 87, 77, 32, 78,125, 65, 79, 87,129, 30, 46, 73, 59,189,123, 30,
 32,125, 83, 81,128, 79, 30,194,180,193,255, 59,193,125,129, 74,123, 80, 18, 67,
123,129, 80,125, 30, 65,123, 74,125, 77, 78, 80, 73,123,194, 80, 46, 77,143, 69,
125,123, 30, 82, 14, 46,125,148, 78,130,116,138, 73, 46, 64, 79, 81, 82, 22, 44,
110, 69,194, 73, 77, 81, 78, 83, 65, 81, 78, 73, 82, 77,116, 16, 82, 81, 32,194,
 30,188, 18,194,138, 82, 87, 46, 32, 77, 59,123,125, 80,129,143, 30,150,123, 83,
 82,125, 78,129, 16,123,129,125, 83, 30, 20, 67,194,123, 81, 32,178,192, 68,194,
123, 26, 81,125, 20, 73,123,194,129, 46,125, 30, 69,194, 64,129, 79,143, 44, 46,
125, 65, 79, 87,129, 30, 46, 73, 63, 87, 79, 44, 32, 26,129, 16, 26,129, 87, 30,
 44, 46, 62, 87, 32,129, 20, 77, 79,128, 87, 79,125, 83,143,129,125,154,194, 87,
 32, 26, 44, 30,148, 78, 26, 81, 32, 73,138, 64, 79, 81, 82, 22, 44,110,162, 30,
194, 87, 32,138, 46,106, 87, 73,192, 32, 79, 77, 66,194, 82, 77, 81, 14, 20,123,
154,194, 26, 87,114,132, 46,128, 79,180, 30,128,194,193, 83,136, 30, 77, 20, 81,
 82, 90,180,138,194,193,179,192, 55, 83,136, 81, 77, 20, 78,114, 66, 46, 14, 22,
 77, 30, 30, 65,123,194, 73,184,129, 81,150, 82, 83, 81,129,194, 73,114, 79,125,
 24, 82, 81, 18, 55, 82,125,184,194,123, 83, 62,123,194,182,184,125, 83, 97, 82,
125,123, 83, 81,182, 87,128,194,193,179,255,255,255, 65, 86, 94,194, 25, 43, 85,
149, 86, 80, 19, 25,  7, 13, 67,194,123,192, 25,255,255,155, 85,194, 25, 80, 19,
 84,148, 85, 25,192,129, 84,  7, 82, 76, 46, 89, 30, 78, 20, 14,128, 30, 79,178,
194,179,193, 97, 30,123, 89, 46,125, 14, 55, 30, 89,123,125, 14, 46, 66,194, 75,
125, 89, 81, 77, 62, 89,123,125, 20, 30, 81,148,123,154, 32,136, 87,138,194, 46,
183,174, 87, 79,108,128, 66,155, 78,174, 73, 32, 77,128,147, 77, 73, 79, 81, 82,
 78,128, 30, 79,128,180,179,193,  2, 78,174, 73,136, 79, 81, 81, 67,194, 32,123,
 46, 30, 74,  2,123, 74, 80,129, 78, 46, 58, 78,123, 74,129, 46, 30, 79,123,129,
143,125, 83, 80, 68, 78, 32,194, 74, 83,129, 16, 53, 74,194,193, 80, 32, 79,188,
 32, 81, 83, 65, 78, 77, 16,123, 81,194, 80, 78, 30, 58, 78,123,194, 30, 81, 32,
150, 83, 82, 81, 78, 80, 93,147,123, 81,194, 72,125, 77, 67,123,194, 32,178, 81,
192,194,128,129, 79, 87, 81, 83, 46,106,129, 46, 83, 81,143,192,154, 87, 30, 32,
129,143, 46,147, 79, 78, 83, 81, 87,143,  2, 79,129, 30, 73, 46, 78,162, 87,143,
129, 46, 30,123, 73,128, 30,194,178,193,179,255, 69, 71,194, 14, 76, 75, 46, 67,
194,123, 32, 74,178,192, 64,174, 71,129,194, 74, 75, 16,123, 30,194, 74, 26, 80,
149,123, 75,174, 74, 72,194, 78,147, 71,123, 81, 77,194, 79, 64,123,129, 71, 32,
194, 26,106,123,129, 81, 64, 71, 80,149, 14,123, 81, 79,194,180,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,

 69, /* c3-g3 */
125,121, 77, 18, 56, 22, 26, 46, 30, 79, 63,116, 32, 30, 22, 80, 20,108, 80,192,
193,179,187,255, 65, 32,116,138, 30, 26, 22,  2, 63, 32, 56, 81, 44, 78, 66,188,
116, 63, 56, 84, 91, 74,154, 32, 76, 44, 26, 30, 75,162, 76, 32, 53, 26, 67, 44,
108,179,193,180, 72,187,192, 28, 72, 26, 73, 81, 75, 44, 65,116, 72,138, 73, 30,
 26, 18, 72, 67, 26, 73, 30, 75, 80,168, 32, 87, 26, 73, 83, 30, 65, 73, 87, 32,
 26,130, 30,108, 32, 22, 30, 26, 14, 44, 67,116,130,187, 81, 32,194, 63,116, 77,
 87,130, 73,158,149, 81, 26, 79, 77, 66, 73, 79,168, 32, 44, 93, 83, 26,116, 18,
 32, 26, 44, 93, 46, 80, 64,130, 44, 86, 80,110, 46,154, 32, 77, 26, 44, 93, 83,
 66,130, 77, 86, 93, 80,188, 63,116,130, 77, 44, 80, 46, 22,108, 80,192,185,193,
187,255, 67,116,184, 74,130,187, 38,162, 10, 30, 38, 28, 46, 14, 18, 77, 67,  4,
 80, 74, 79, 66, 80,116, 38,130, 82, 67, 68,116,184, 79, 80,130, 83, 44, 64, 79,
 36,116, 60,130, 38, 67,116, 74,130,184,138, 38,108,192, 80,185,193,187,255, 63,
 79,116,130, 77, 28, 38, 66,116, 80, 38,188, 74, 60, 65, 74, 79, 77, 32,116, 80,
148, 74, 67, 76, 22, 26, 30,138,110, 18, 76, 22, 73, 81, 75, 26,  2, 76, 22, 26,
 30, 46, 44, 16, 76, 73,138, 53, 75, 81, 28, 76, 22, 44, 67, 30, 81, 34, 76, 53,
 22,138, 30, 75, 53,  2, 26, 74, 32, 60, 22, 14,101, 52, 81, 30,138, 74, 26,  4,
 52, 32,110,130, 26, 73,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255, 32, 66,138, 85, 38, 73, 80, 94, 63,110,116,130,138,
 16, 26,119, 85, 26, 59, 40, 86, 80,  2,130, 85,138, 84, 16, 59,152, 59, 73, 38,
 86,192,130,101, 59, 85, 73,130, 86,138, 82,115, 32,192, 81, 78, 54, 75, 67, 32,
187,116, 81,194,130,101, 32, 81, 75, 78, 26, 68,  4, 32, 81, 80, 75, 54, 83,121,
 54, 81, 78, 68, 75, 83,  2, 32, 75, 30, 78, 26, 83, 67,115, 53, 81, 32, 26, 68,
192, 67,116,192, 32,130,194,187,  2, 74, 64, 63, 44, 22, 81,  4, 53, 81, 60, 32,
192, 26, 55, 30, 81,192, 53, 65, 74,119,178,192, 64, 32, 26,194,192,121, 78, 32,
 53,130, 80, 88,115, 78, 53, 82,184, 67, 83,  2, 78,130,176, 77, 83, 80, 64, 22,
194, 44, 53, 67,184, 68, 46,138, 82, 67, 53,176, 66, 78,138,180, 67,194,195, 65,
116, 67,123,127, 22,109, 32,129, 18,123,117,113, 80, 32,103,148,121,131, 74, 79,
129, 67,164,121,125,131,111,101,129,149,121, 67, 30, 74, 77, 79,255,255,255,255,
255,255,255, 30,121,116,130, 79, 74,184, 80,115,116,180, 79,187, 67,110,119,116,
 38,110, 14, 46, 83, 63, 38, 36, 79,184, 82,116, 72,116, 80, 79, 74,130, 60, 68,
116,184, 24, 79,154, 82,130, 69, 32,117,125, 75, 46, 14,119,121, 72, 75,125, 73,
117, 16,117,125,135, 69, 32,193, 67, 72,125,192, 75, 71, 73, 68, 72, 32,125, 73,
 14, 22,115,121, 72,180,117, 71, 75, 74, 72,116, 30, 73, 67, 60, 53,121,116, 72,
138, 73, 30, 26, 66,116, 20,130, 75,192, 32, 68, 26,116, 22, 75,188,138, 64, 72,
116, 71,110, 67, 70, 18, 76, 72, 67, 73,188, 46, 67,121,130,193, 74,187,194,138,
119, 46,187,130,116, 81, 68, 66, 74,130,193,116, 81,192,101,130,110, 74, 81, 53,
138,148,130, 74,138,110, 30, 81,  2,138, 74,130,179, 60, 20, 46,121, 30, 79, 22,
 80, 14, 34, 72, 74, 79, 80, 67, 53, 77, 18, 67,130,194, 79,138,154, 66, 38,188,
194,116,130, 74,115, 77,180,116, 53, 30, 79,  2,130, 38, 60, 30,138, 82,119, 67,
128,178,194,193,179,187, 30,148,178,192, 64, 32, 26,194,  2, 32, 26, 22, 46,194,
 60, 22, 81, 26, 32, 60, 74,193,106, 32,178, 68,192,154, 26, 16, 30, 81, 26, 32,
194, 22,194, 68,138, 82,195,193,255,255, 18,116, 79, 82,190, 30, 32,128, 67, 82,
 80, 46, 88, 32,114, 82, 77, 32, 46, 79,192, 34,116, 79, 82, 32, 46,138, 65, 46,
 78, 95, 77,159, 32, 77,148, 78, 30, 32, 81, 26,138, 65, 80, 81, 78,116, 82, 30,
 66,188,194, 82,180, 80,138, 68, 81, 82, 30, 26, 83, 22, 63,116,194, 82,187, 81,
 78, 16, 81, 82,192, 22, 80, 26, 80,154,194, 87, 32, 26, 44, 30,148, 78, 26, 81,
 32, 73,138, 64, 79, 81, 82, 22, 44,110,162, 30,194, 87, 32,138, 46,106, 87, 73,
192, 32, 79, 77, 66,194, 82, 77, 81, 14, 20, 78, 64,194, 79,116, 81, 71, 30, 65,
194, 81, 80, 71, 82, 77, 18, 14, 32, 81, 30, 71, 26, 22, 81, 71, 82, 32,194, 77,
 34,179, 14, 82, 30, 81, 80, 66,180,188, 82, 77, 30, 80, 30, 65,116, 38,110, 14,
 46, 83, 64,194, 67, 82, 38,116, 36, 26,184, 60,116, 18,194,182, 18, 82,184, 22,
 24, 42, 46, 66,188,184,180, 82,116, 36,106, 24, 22,192,  6, 79,184, 68,116, 18,
117, 32,123,113, 22, 80,148,131, 46, 22,101,121, 67, 67, 32,122, 30,187, 46,192,
164,174,131,121, 30,125,101,149,121, 46, 77,125, 30,154,163,101, 32,154,174, 79,
 46,154, 63, 14,161,255,255,255,255, 65, 79,116,194, 67, 80, 46, 18,116, 32, 30,
 79, 22, 80, 16,116,182, 60,189,178, 32, 66, 79,194, 78,182,147,189, 26,116, 30,
 79, 78, 32, 60,192, 96, 95, 46,255,255,255,255,121,184, 79,195,190,116,176, 75,
 77, 83, 80, 79, 44,176, 65, 79, 30,194,176,189,190,119, 79, 30,195,189,176,190,
162,138, 67, 82, 46,189,195, 32,119, 86,116, 38, 80, 73, 40,115, 86,116, 38, 73,
 24, 40,121,116, 86,130, 73, 80, 26, 34,154,116, 85, 73,130, 26,101, 86,130, 73,
 84, 85, 26,  2, 73, 86,130,116, 85, 84, 78,148, 82, 83, 81,116, 80, 26,121, 71,
116, 79, 26, 32, 20,119, 30, 32, 82, 14, 81, 26, 69, 32, 30,154,192, 79, 80, 65,
 81, 80, 77, 79, 71,130, 16, 81,116, 32, 82, 71, 30, 74,119, 67,116,188, 75,138,
 81,115, 73,116, 67, 81,138, 72, 75, 71,116, 72, 60, 81, 46, 66,116, 20,130, 75,
192, 32,121, 72,116, 26,138, 44, 30, 64, 72,116, 71,110, 67, 70, 18,154,  2, 79,
116,194,178,182,189, 68,116, 32, 30, 79, 22, 80, 26,116, 30, 67, 79, 78,138, 62,
 79, 80, 46, 88,182, 30, 10,116, 67, 79, 78, 83, 95, 30, 30, 67,116, 79,178,194,
 78,121, 71, 79, 26, 14, 32, 22,119, 14, 32, 81, 30, 71, 26, 30, 32, 71, 77, 79,
178,116, 24, 32, 81, 71, 30, 79,192, 36, 32, 30, 22, 26, 71, 79, 10, 32, 30,154,
192, 79, 80, 32,121,130, 80, 84, 86, 24, 85,119, 73,192, 38, 85,194,130,115, 84,
 73, 85,192,194, 40, 30,130, 38,192, 73, 85,194, 36, 86,192, 85,130, 73, 80, 42,
 73, 86,130,192, 85, 38, 67, 20,130, 81,194,187,192,255,121, 60,116, 53, 32, 44,
 22,115,192, 32, 81, 60, 74, 22, 16, 60, 32,116, 26, 30, 68, 26, 60, 32,154, 26,
 68,178,  8, 32, 60, 14,116, 46,178,193,121,116,191,190, 67, 30, 32,148, 67,192,
194,190, 88,116,  2,192, 79,130,190,194, 78, 34,192, 83,138,194,255,255, 10, 67,
 79,192, 83, 78, 77, 76,131,192,123,194, 79, 14, 30,121, 18, 79, 74,130, 67, 80,
119, 82,184, 22, 24, 42, 46,115, 79,184, 46, 24, 82, 36, 68,154,184, 79, 82, 18,
 42, 10, 24, 22,192,  6, 79,184, 62, 82, 24,184, 80, 79, 42, 32,115, 86,150,125,
 87, 65, 93,  8, 38,148, 85,125,123, 16, 38, 40, 16, 65,125, 72, 16,143, 84,128,
 79,178,185,194,193,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
 24,128,194,178,179,193,255,255, 67,194,192,123, 32, 18,180, 22, 40, 86, 18,  0,
194,129, 83, 86, 40,143,194,129,172,124,194,179,192,193,255,255, 97, 40, 86, 73,
180, 32,123, 73,128,185,178,193,194,255,255, 22, 72,125, 76,183, 24, 71,150,125,
170, 76, 71, 40,123,  2,183,125, 72,194, 76, 24, 67,123, 72, 74, 40,183, 71, 16,
125, 72,143,123, 76,170,129, 16,192, 40, 84,116, 38,124,128,194,193,187,255,255,
255,150,114,138,124,134,120,142, 55, 40, 86,194,192,187, 20, 22,116, 86,187,194,
 48,193,149, 86,114, 26,124, 85, 84,194,150,170,190,178, 80, 40, 24,149, 40,190,
 80, 26, 86,189, 16, 73,192,170, 86,190,125, 22, 86,125, 84, 73, 80, 89, 28, 86,
 80,129,123, 89,125, 18,125, 73, 89,129,123, 84, 84,128,194,178,185,193,255,255,
 28, 86, 77, 40,123, 85,194,  2, 86, 63, 56, 77, 40,123,150, 49,125,170, 77, 38,
 44,124,194, 26,192,185,193,255, 22, 56, 77, 40,183,194,123,121, 86,162, 72, 85,
123, 93,125, 26,154, 84,125, 72,129, 93, 87, 65, 72, 40,125,129,143, 84,168, 72,
123,143,129, 24, 26,164, 72, 40,125, 79, 87, 93,108, 72,129,185,193,192,255, 40,
 66,121, 80,125,145,117,135, 68, 86, 73, 34, 94, 80,125,108, 80,121,192,193,255,
255, 90, 80, 86, 73, 84,115, 94, 26,117, 86, 84, 73,115, 80, 18,121, 73,115, 34,
 86,135,  0,108, 80,192,129,179,193,255, 67,123,192,194,178, 32,255,168, 84,  8,
 24, 16, 73, 32,106,123, 85, 84, 80, 73, 94, 65,129,123, 85, 84,193, 94,149,  8,
 32, 84, 24,123, 40, 26,108, 80,192,129,179,193,255, 67,123,192, 18,194, 32,178,
 66, 73,129, 38, 80, 86, 84, 18, 18, 86, 73,125, 80, 84,149, 80,123, 73,125, 38,
 32,154, 84, 59,129, 80, 10, 73, 73, 83,182,125, 26, 20, 74,143, 63, 38, 44,123,
125,129,143, 64, 38, 44,123,125,143, 26, 97, 40,143,123,129,125, 26, 90, 40, 74,
 75, 76, 26, 80, 55,125, 40,123, 74,129, 26, 84,108,185,193,192,129,255,255, 28,
 86, 77,125, 26,129, 40, 83, 56,125,184,143,  0, 26, 63, 86, 77, 38, 24, 85, 44,
 67, 56, 40, 77,143,125, 49,164,125, 87,129, 40, 63, 70,119, 86, 65,129, 72,174,
194,125,143, 16, 65,125,194, 38,185, 72,150, 65,125, 72,123,194, 40,148, 85,125,
194, 79, 40,143,164, 65,125, 40,154,194, 72,114,125, 40,129, 93, 72, 85,194, 65,
 86,143, 40, 94, 84,129, 16, 89, 26, 40, 80, 86, 20,134, 26,129, 24,125,189, 88,
 64,143, 86,129, 85, 94, 73,128,129, 84, 24, 26,125, 86, 76, 80,129, 24, 26, 88,
 86, 24,128,194,178,179,193,255,255, 97, 40, 86,123, 80, 94, 84, 65, 40, 73, 94,
125, 84, 86,150, 18,194, 30, 73, 16,123, 67,194,192, 18,178, 32,123,106, 85,125,
 86, 84, 18, 32, 38,128, 30, 79,194,178,193,255, 65,123, 74,125, 77, 78, 80, 66,
123,194, 74, 77,125,193, 67,194,123,125, 22, 30, 14, 68,123,194, 74,125, 77,193,
 83,123, 30, 22, 46, 14, 82, 73,128,185,178,193,194,255,255, 66, 72, 24, 26, 20,
 80,129, 65, 72,125,143, 38, 74,123, 67,194, 74,125, 72,123, 40,150,123,194, 75,
125,129, 26, 64,174, 72, 59,194,143, 44,129,128,194,193,187,255,255,255, 65, 86,
120, 26, 40,116,194,149,114, 86,194, 26,120,180,154,180, 40, 84,194, 80,187, 16,
 40,116, 38,124, 86, 84, 64, 86,174,194,116, 80, 94, 65,129,119, 86,120, 26, 40,
116,194,115,116, 86,120,194,180, 40, 86,116,120,192,187,194,193, 79,120,194,116,
 85,187, 84, 69, 86,116, 85, 40, 84,194, 72,120, 24,116, 86, 85, 38,125,115,116,
188,194,180, 86, 73,121,116, 73, 86,130, 85, 84,119, 26, 86, 38, 40, 73, 24, 86,
116,154, 48, 40, 26,194,  2,116,130, 86, 38, 73, 40, 79,116, 38,187, 73, 84, 85,
 84,119,194, 86, 24, 38, 85, 26, 86, 86, 38,194,178,192,193, 26,174,129,125,143,
 86, 24, 72, 24, 85, 56, 86,125,129,150,123, 87,129, 38,194,125, 18,129,125, 86,
143, 87, 24, 38,121, 77, 74,123, 79,129,125, 79,129,125, 80, 82,123,174,115,129,
 77, 30,180, 79,123,119,123, 74,125, 77, 78, 80, 86, 80,123,125, 77,129, 79, 67,
123,125, 74, 30, 22, 46, 40, 86,117,121,125, 34,115,174, 26,117,135,174, 86,125,
 66,119,121,117,194,125,115,143, 66, 86,135,117, 32,125, 80, 18,135,117,115, 86,
 46, 48,150,121,125, 32, 34, 46,143, 85,119, 86, 38, 24, 84, 78,125, 86, 38, 86,
194,192,178,193,115, 86, 84,143, 92,125, 38,148,129, 24,143, 71,125, 78,149, 87,
 38, 84,125,123, 86, 26,129,125,154, 86, 84,174, 16, 86,119, 65,125,194, 38,185,
 72,158, 79, 72, 85, 40, 65,125,150,123, 87, 79, 85, 65, 40,101,125, 65, 16, 84,
 93, 24, 66,129, 24,125, 16, 72, 93, 65, 72,129, 79,194,193, 87,125,115, 38, 26,
 73, 86, 84, 40,119, 38,130, 73, 86, 94, 80, 66,116, 38, 86,130, 26, 73, 22, 73,
192,116,130, 38,194, 65,130, 38,154, 26, 94, 40, 28, 86, 73,116, 85,192,130, 38,
115,125, 82, 74, 83,170, 67, 67,125,194,123,193, 74, 30, 62,125, 60, 22, 74,192,
 30, 10,125, 32, 30, 83, 14, 78, 83, 60,125, 82, 78,143, 79, 24, 22,125, 30, 60,
 46,178, 73,115,125, 72,143,123, 76,170, 65, 72,123, 38,143, 87,193,119,125,123,
 76,143, 26,184, 83,129, 74,143, 75,123,125, 90, 76,194, 75, 74, 72, 24, 22,125,
 72,129, 74, 24, 75, 26, 67,194,123, 18,192,178, 32, 66,129,123, 38,125, 18, 86,
115,125, 84, 38, 73, 80, 34, 24,125, 86, 73, 32,178, 34, 28, 59, 86,123,125, 73,
 32,  4, 59,129, 86,123, 73,125, 80,115, 81, 77,125,183, 79,184, 65, 79, 87,125,
 81, 73, 26, 67,125, 81, 73,184,129, 77, 10, 81, 79,125, 78, 73, 40,119, 81,125,
 78, 73,194, 77, 24, 81, 79,129,125, 78, 87, 63,129,149, 86,116,114,124, 85,120,
119, 86,174,194,114, 84,116, 69, 86,116, 85, 40, 84,194, 84,174, 24,120,134,116,
 20, 70, 24, 86, 26, 85,120,116, 77, 86, 24, 26, 85,120, 40, 86,148,129, 87, 85,
125, 44, 24, 65, 72,129,194, 79,193, 84, 16,129,125, 38, 16,143,174,150,123,125,
129,  8, 87, 40,164, 65, 40,174,125, 87,  8,121, 72,125,129, 40, 44, 26, 85, 65,
 84, 86, 87,129,143, 24,115,180, 86, 57, 84, 92,123,121, 86,129,143, 57, 16,  0,
119, 86, 24, 57,194, 64, 92, 22,129, 71, 86,125, 24, 40,101, 86, 40,143, 24, 87,
125,125,121,116, 86,130, 38, 73,187,119, 86,194, 73, 38, 24, 80,115, 86,180,188,
 73,194,187, 84,116, 86, 40,130, 26,187, 18, 86,130, 24, 73,138, 26, 70, 86,116,
 26,130, 24, 85, 38,121,123, 79, 74,125,129, 80,115, 79,180, 77,123,143, 78, 67,
123,125, 74, 30, 22, 46, 65,123, 67,129,125, 60, 79, 66,123, 79, 32, 74, 30,129,
 64,123,129, 79, 80,125, 74,143,121,130,134, 85, 86, 26, 73, 69, 86, 85,138, 73,
 84,134, 66,138,130,134, 24, 86, 84,150, 16,130,138, 26,194, 66, 77,134, 86,130,
 73,194,138,148,128, 16,134,138, 73, 94, 30,121, 79, 18,125, 86, 24,184, 40,  0,
 83,125,184, 20, 26,143,  0, 28,125, 77, 24,184,  0, 80, 64, 78, 38, 86, 44,129,
174, 67,184, 40,192, 77,174,182, 63, 86, 44, 77, 38,178,129, 74,154,125, 76, 73,
 46, 75, 24,162, 73,125, 76, 67, 81, 24, 63, 72, 36, 38, 46,129,143, 64, 72, 22,
 36,129, 46, 42,108, 72,129,185,193,179,180, 83, 72,129,143,125, 42,  6,125,108,
192, 80,185,193,187,255, 63,116, 38, 80,130, 36, 46, 67,116,184,130, 74,187,182,
 64,116, 22, 36,  6,130, 38, 65,116,130, 79, 74,184, 80, 66,116, 80, 24,188, 38,
 22, 24,108, 80,185,192,129,193,255, 63, 77, 80,123, 78,129, 83, 64, 78, 36,174,
 74, 32, 80,168, 80,125, 79,129, 32, 82, 18, 80, 79,125,184, 83, 82,162,125, 78,
 80, 67, 79, 83, 80, 83, 79,184,143, 82,158, 24, 63, 38,123,125, 36, 73, 24, 64,
 38, 36, 22, 79,123,143,148, 79, 24, 36,123, 12, 14,108,125, 79, 18, 22, 46, 24,
162, 79, 66, 24, 73,158, 36, 12, 83, 20, 82, 53,125, 77, 78,108, 80,185,192,129,
193,255, 63, 77,123,129, 80, 78, 53, 18, 18,  6,125,  4, 20,123,168, 74, 79,125,
 80, 24, 20, 66,129, 80, 24,174,123,180,115,125, 68, 38, 24, 12,110,116, 36,148,
 24, 18, 38, 53,184, 12,168, 18, 38, 46, 74, 36,130, 22,184, 79, 38, 60, 46, 18,
 62,183,184,  6, 24, 12, 42,124,194,192,185,193,187,255, 80,124,192,179,185,193,
194,255,148, 83, 38, 18, 46, 81,184, 55,184,125,183, 18, 81, 83, 16,125,183, 81,
129,184, 18, 90,184,183, 38,182, 18,185, 97,183,184, 46,182, 18,180, 46, 18,125,
182, 77, 74, 67,185, 68, 38, 77, 14, 74, 22,125, 67,125,123,183,143,129,182, 62,
 77,125, 74,183, 82,182, 83,125,129,183, 82, 30,182,  2,183,125,182, 30, 67, 82,
185, 76, 79, 67, 74, 83, 80,183, 18, 79, 24,183, 74,125,184, 68,191, 74, 82, 24,
143,123,154, 74, 60, 67, 53,125, 79,106, 79,184, 42, 24, 74, 67, 67,123, 79,129,
125,143, 74, 79,128,178,185,193,194,180,255,150, 77,  8, 65,123, 80, 38, 16, 65,
125, 77, 72,183, 86,148, 78, 80,123, 86,125,185,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,182, 76,123, 74, 24,185, 22, 88,130, 24, 14, 38, 22,  6,
123,162, 79,184, 83, 38, 46, 82, 16, 74, 67,125,176,183, 38, 22, 60, 79,184,183,
176, 24,  4, 74, 79,176,143,184, 24, 63,123,121,128, 38, 80, 83,136, 24, 67,128,
 79,184, 38, 80,192,115,128,180,187, 80, 46, 38, 22,128, 80, 82, 83,174, 38, 84,
180,194, 38,178, 36,136, 69,184,128, 83, 79,192,180, 38,121,123, 79, 74,125,129,
 80,115, 79,180, 77,123,143, 78, 67,123,125, 74, 30, 22, 46, 65,123, 67,129,125,
 60, 79, 66,123, 79, 32, 74, 30,129, 64,123,129, 79, 80,125, 74, 36,121, 79,129,
 74, 80,184, 77, 65, 30,125,129, 79, 74,123, 69, 24,125, 79, 30, 42, 28, 67,194,
125,123, 30, 42, 28,119,194, 24,123,125, 77, 78, 77,129,125, 78,123, 79, 74, 79,
121, 86, 44, 77, 38,178,129,148,129, 86, 26,125, 78,143, 16, 65,129,174,143, 86,
 38,164, 65,174, 80, 77,129, 40,150,123,129, 38, 81, 44,125, 65,129, 72,125, 24,
194, 65, 80,121, 38,123,125, 36, 73, 24, 84, 38, 36,194,193,192, 87,148,123, 38,
 73, 83, 24, 94,150,123, 87, 79, 81, 38,129,149,123, 81, 79, 36, 18, 83, 67,123,
194, 81,192, 18,185,125,121,116, 38, 80,130, 36, 46, 84,194, 38, 36,178,192,193,
115,180, 79,  6, 46, 14,110,  2, 36, 79, 38, 22, 60, 46,148,130, 38, 53, 60, 36,
 74, 70, 82, 79, 83, 80,116,194, 64,123, 67,128, 79,184, 38, 80,192, 71,180, 80,
 79,128,174,187, 85,180,194,192, 38,178,136,115,128,180,187, 80, 22, 36, 69,184,
128, 83, 79,192,180, 63,128, 80,174,184,180,136, 79,121, 78, 38, 86, 44,129,174,
 16, 65,129,174,125, 86, 16, 65,129, 72,125, 24,194, 65,150,  8,125,123,129, 38,
 81,164, 40, 80,174, 65,123, 38,148,162,123, 51,129, 38, 78, 36,121,125,129, 79,
 74,123,184, 65, 30,125,129, 79, 74,123, 69, 24,125, 79, 30, 42, 28,115, 77,123,
125, 74, 80,143, 67,194,125,123, 30, 42, 28,119,194, 74, 79,123,125, 83, 22,115,
125,180,123, 60,143, 67, 63, 38, 77, 28, 74,123, 78,119, 79, 60,123, 67, 83,125,
 67,125,123, 30, 28, 74,129, 65,129, 30, 38, 79, 74,125, 69,125, 74, 79, 30, 60,
 83, 38,121,123, 79, 80, 74,129, 77,115,123, 77, 80, 83,143, 22, 67,123,125, 74,
 30, 22, 46, 65,123, 67,129,125, 60, 79, 71,123,129, 79, 67, 32, 53, 66,123, 79,
 32, 74, 30,129,174,121, 24,160, 79,143,129, 42, 65,123, 79, 67, 24,129,160,  4,
 18, 79, 60,192,171,123,115, 24, 79,180, 36,160, 83,101,160, 79,129, 24, 18,192,
 67, 79,123,143, 24,192, 18, 65,129, 69,184,138, 22, 46, 24, 36, 63,134, 38, 36,
 24, 46, 18, 72, 74,116, 67,120,192, 80, 18,120, 18, 67, 83, 82, 38,101,120,192,
184, 38,182, 80, 68,120,116, 24,184, 46, 80,125,121,116,130, 79, 74,184, 80,115,
116,180, 79,187, 67,110,119,116, 38,110, 14, 46, 83, 63, 38, 36, 79,184, 82,116,
 72,116, 80, 79, 74,130, 60, 68,116,184, 24, 79,154, 82, 80,115,129, 79, 73, 24,
123, 81,121,123, 79,129, 73,125, 38, 72,125,129, 73, 24, 79,143,119,123,194, 73,
184,129, 81, 79, 24,129, 40, 22, 79, 10, 67,123,194, 81,192, 18,185, 79,119,174,
194, 72,185,193, 86,148,129,192, 38, 81,194,125,121, 86, 77,194,125, 44, 78,149,
123,129, 40, 72,194,143, 72,129,125,193,192, 24, 72, 18,129,154, 65, 72,125, 40,
123,115, 79,128,187,136,132,110, 67,128, 79,184, 38, 80,192, 69,184,128, 83, 79,
192,180, 16,128, 80, 12,184, 82,138, 63,128, 80,174,184,180,136, 34,128, 82,188,
 80,174,154, 38,121, 77, 74,123, 79,129,125, 79,129,125, 80, 82,123,174,115,129,
 77, 30,180, 79,123,119,123, 74,125, 77, 78, 80, 86, 80,123,125, 77,129, 79, 67,
123,125, 74, 30, 22, 46, 83,154, 81, 14, 76,143, 69, 24,125, 76, 24, 74,125, 79,
123,182, 69, 79,125, 18,129,178,123, 18, 79,123, 24, 80, 46,143, 34,123,125, 80,
 79,182, 46,  4, 79,123, 60,125, 24, 22, 79,121,125,184, 20, 26,143,  0,164,154,
125,184, 40, 38,183,148, 16, 80,129, 72, 77, 40, 16, 65,194,129,125, 24,184,150,
 81,123,  8,129, 40,125,255,255,255,255,255,255,255,123,115, 79,183,187,128,182,
184, 69,184,128, 83, 79,192,180, 28, 82,154,182,194,187,128, 22, 82,154,194,187,
184,128, 90,194,192,180,138,178,193, 18,154, 82,184,187,182,194,125, 90,138,178,
194,192,193,255,121, 79, 46,184, 74, 22,194,  2, 79,138,184,182, 60, 12, 97,154,
184,187, 79, 83, 24,101, 79, 46,182, 12, 38, 36, 34,116, 80,154, 82, 24, 18,182,
121,123, 46,143,176, 24,129, 81,154,143, 24, 76, 38, 14, 69,125, 74,184, 79,176,
 24,115, 79,129,176,183,123,143, 82,123, 79,154,143,189,190, 76, 24,123, 79,184,
176, 83,184, 81,129,123,143,190,160, 76,121,123,129, 80, 79,125, 14, 69,123, 79,
 80,125,129, 74, 76,123, 80, 79, 83,129, 74,  2, 79,123,143,176, 80,182,162, 67,
 80,123, 74,143, 79, 83,115,125,128, 30,193,194,178,179,187,168, 76, 81,110, 44,
 80,138, 22, 81, 77, 26, 82,116, 80,148, 81, 78,192, 76, 14,116,120, 79, 77,192,
 78,116,194,124,192, 26,194,187,193,179, 77, 83, 56,129,125, 20,123,143, 62,125,
 32, 20, 56, 26, 46, 97, 56,125, 46,129,123,143,150,125, 49, 32, 46, 84, 56, 67,
192,123, 32, 81,194,180, 22,125, 32, 56,123, 22,129, 76,128,194,193,178,179, 30,
255, 67,192,194,123, 74,180, 32, 26, 32,123, 26, 83,125, 97, 68, 73, 72,125, 20,
 74, 71, 16,125,129, 32, 22,123, 14, 22,123,125, 46, 32, 72,129,123,124, 26,180,
193,194,192,179,128, 30,178,128,180,193,187, 16,193,132, 76,136,138, 82, 83,187,
 62,136, 76, 55, 68, 26,193,136, 76, 30, 14, 22, 22,193, 76,136, 77, 26, 81,129,
128,194,193,187,179, 30,255, 18,120, 26,138, 80,180,192, 16, 76,116, 26,120,138,
 32,154,192, 22, 76, 80, 46, 26,162, 76, 26, 80, 22, 44, 32,150,114, 46,192, 44,
 76, 22, 30, 68, 82,123,125, 24,184, 18, 26,125, 24,184,183, 18,123, 83,183,185,
182,192,143,184, 16,183,184, 18, 24,182, 76, 67,123,185, 18,192, 81,178, 22, 24,
183,  6, 18, 42,184,119,123,128, 30,178,128,180,193,187, 16, 76,136, 30,132, 22,
 46, 18,136,193,138, 76, 82, 14, 26, 80, 30, 81,136,180, 77, 66, 30,136,174, 81,
193, 79, 28, 30,136, 14, 82, 69, 46,125,128, 30,193,194,178,179,187,148, 78,138,
 14, 81, 46, 76, 64, 79, 81, 82, 22,187,138,155, 81, 80,194, 78, 32, 76, 65,116,
 81, 80, 78, 82, 77, 68, 82, 81, 80, 77, 78, 30, 22,128, 30,194,178,193,185,179,
148, 38, 14, 30, 16, 76, 46, 16, 82, 30,129,125, 14, 28, 65,123, 78, 38, 77, 81,
125, 68, 30,123, 38, 78, 14, 62, 26,194,125, 30, 81,123, 80, 30, 65,123, 80,125,
 81,143, 38,148,184,194, 18,182, 81, 24, 83,123,143,192,193,182,185, 66,123, 24,
125, 80, 82,143,114, 24,184,129,125, 80, 82, 64,194,123, 22, 36, 38,143,129,128,
194,193,187,179, 30,255, 67,194,116, 32,187, 81,192, 18, 26,194,138,120, 80, 69,
 66,120, 26,194, 82,180, 80,154, 26,194, 30, 22, 76, 69,148, 26,194,114, 76,138,
124, 14,128, 30,178,194,193,185,179, 63, 78, 80, 12, 38, 82, 77, 83,123,129,143,
194, 38, 76, 64, 22,123, 38, 78,143,125, 16, 82,125,129, 30,123, 81, 18, 30,194,
 38, 78,125,123, 16,125,148, 76, 81, 82, 80, 26, 14,168, 81, 76, 32, 44, 79, 30,
 24,154, 81, 79, 77, 78, 82, 62, 81,116, 80, 77, 26, 79, 10,154,192, 80, 79, 78,
 77, 22, 81, 22, 79, 77,192,116, 81,115,125, 82, 74, 83,170, 67, 67,125,194,123,
193, 74, 30, 62,125, 60, 22, 74,192, 30, 10,125, 32, 30, 83, 14, 78, 83, 60,125,
 82, 78,143, 79, 24, 22,125, 30, 60, 46,178,194,115,190,191,192,170, 26, 77, 10,
 30, 79, 44, 22,190,129, 65, 79, 46, 77, 69,129, 76,148, 69, 76, 30,190, 14, 26,
101, 26, 44, 30,125,190, 69,119, 26,125,129, 22, 80, 76, 76,115,125,129, 32, 22,
123, 14, 24,178,154,123, 69,193,192,148,123, 69,129, 74, 75, 30,150,123, 26, 75,
125, 74, 73,119,123, 74, 75,125, 32, 44, 10,129, 69,123, 72, 32, 73,123,115,193,
132, 76,136,138, 82,119, 76,136, 30,132, 22, 46, 48, 79,136,132,193, 97,128, 24,
 79, 76,193,136,138,178, 22, 82, 30, 69, 76,132, 77,  4, 79, 82, 30, 69,193,132,
 82,115,125, 81, 68, 77, 75, 54,121, 32, 83,125, 30, 75,123, 67,123, 32, 81,192,
178,194, 68, 32, 81, 75,123, 83,125,119, 30,125, 75, 22, 14, 46, 65, 61,123,129,
 81,125, 54, 67,123,121, 79, 77, 26, 20, 80, 81,  4, 79, 30, 62, 22, 26, 81,115,
 77,193,192, 20, 82, 78,  2, 62, 79, 30, 77, 78, 22,119, 30, 82, 80, 69,188, 77,
 60, 30, 69,188, 79, 77,193, 81,115,125,123,143, 30,129, 74,121,123, 74,125,143,
 44, 30, 74, 32,125,129,180, 46, 30,  4,129,123,125,193, 74, 30, 16,125,194,123,
193, 74, 30,119,194,123,125, 22, 30, 14,192,121,184, 79,190, 30,191,174,119,176,
 30, 78,174,190, 79, 65, 79,176, 44,193, 26,190, 63, 79, 77,190,174,194,193, 68,
 79, 76,125, 78,190, 77, 64, 79, 78,174, 30,194, 44, 32,121, 40,125,129,143,182,
184,115,125,183,143, 62,129,185,119,194, 76,125, 81,129, 62, 53, 76,125, 81,143,
 40,129, 60, 69,129, 76,125, 81,143, 63, 38, 76,125,129, 44,143,194,121,190,191,
189, 20, 30, 14,115, 79,188,190, 80, 76,143, 65, 79, 46,125,129, 77, 20, 81,190,
 81,192, 76, 22, 69, 63, 79, 44, 46, 77,129, 76, 74, 79, 77,188, 46, 78, 20,178,
 18,179,255,255,255,255,255, 81,179, 81,171,255,255,255, 66,172,179,255,255,255,
255,121,184,190,176,177, 30, 14,119,176, 30,190,194,160, 62,115,176,190, 30,194,
123, 14,121,129,108, 22, 30, 14, 26, 76, 46, 18, 26, 30, 44, 46, 14,120, 66, 62,
 69, 26, 20, 81,120,150,114, 22, 32, 44, 26, 30,149, 32, 81,134, 26,124, 14,155,
 32, 26, 80,116,124, 76, 22, 63, 77, 28, 30, 80, 38,123,108,129, 80,185,193,192,
179, 67,123,194,192, 81,178,255, 68,129,125, 76, 69, 62, 79, 26,125, 76, 80, 81,
184,123, 66,129, 80,123,125, 62, 77,125,108, 80,193,187,192,179,255,168, 44, 32,
 79,116, 26,130, 64, 22, 80, 77, 79, 82, 76, 65,116, 81, 80, 32, 77, 79, 18, 26,
 79, 80, 44, 78, 77,154, 77, 26, 79, 30, 22, 78, 46, 67,123,192, 81,178,194,255,
 63, 79,194, 77, 30, 38,129,108, 80,129,185,192,179,255, 68,129, 22, 77, 30, 14,
 79, 64,194, 22, 38, 79, 76, 77,168, 30,123, 79, 76,125, 80, 14, 63, 77,123,129,
 44, 12, 80,108,129, 80,192,185,193,179, 67,123,194,192, 81,178,255, 64, 22,123,
 38,129,143, 30,154, 22, 30,  2, 38,123, 77, 68,129, 22,125, 69, 46, 81, 44, 67,
123, 32, 81,192,178,194,108,129, 80,185,192,193,179, 28,129, 77, 76, 26, 32,125,
 18,129,184, 76, 77, 26,125, 83,129, 20,184, 26,125, 67, 55, 79,129,125,184,123,
 76, 65,123,121,136, 62, 77, 79, 80, 26, 72, 62,188, 79, 77,180,136, 67, 81, 78,
 79, 80, 22, 82, 66, 79,136, 78, 81, 30, 77,115, 79,136, 77, 62, 80,132,119,136,
 79, 81, 62, 78, 80,129, 67,116, 32,194, 81,192,187,162, 76, 80, 69, 97, 46, 26,
 69, 76, 69,120,116,192,193,121, 76, 62, 26, 20,120, 69,101, 76, 69, 97,124, 62,
 80, 68,116, 76,120, 82, 26, 62, 79,121, 77,123, 32, 86, 44,129,119,123,194, 81,
 72, 86, 78, 72,129,123,125, 72, 32,158, 18,129, 83,123, 77, 32, 82, 26,129,123,
174, 32,125, 72,  2,129,123, 32, 83,174,194, 81,121, 77, 74,123, 79,129,125, 79,
129,125, 80, 82,123,174,115,129, 77, 30,180, 79,123,119,123, 74,125, 77, 78, 80,
 86, 80,123,125, 77,129, 79, 67,123,125, 74, 30, 22, 46, 80,121, 77,123, 73, 79,
 30, 87, 67,123, 81, 32,192,194,255, 16,123,125, 87, 73, 81, 83, 72, 73,125, 79,
 87, 30, 77, 18,123, 73, 83, 30,129, 87,115,125, 77,123,129, 87, 73, 82,121, 77,
 79,129, 80, 75, 68,115, 77,125, 79, 75, 78,123, 67,123, 81, 32,192,194,178, 66,
123, 79,129, 81, 78, 83,119,125, 81, 78, 77,123, 75, 72,129, 79,125, 77, 89,123,
 14,121, 79, 67,123,192,194, 81,178,255, 83,129,125, 82,184,182,123, 66,129, 80,
 86, 93,123, 83, 65,123,184,125, 22,129, 80, 68, 30,129,123,125, 22, 81, 62,184,
129,123, 80,125, 81,184, 63, 77,160, 12, 78,129,143, 28, 78, 83, 22, 79, 38, 46,
 65, 79, 30, 22, 95, 67, 38, 66,129, 80,123,143, 78, 38, 90, 46, 38, 74, 95, 53,
 88, 76, 79, 78,123,129, 80, 22,125, 63, 77,116, 80, 78,130, 79,108,192, 80,185,
193,187,255, 64, 22,116, 78, 38,130, 46, 65,130, 30, 74, 53,116, 79, 66,130, 38,
 80,116, 74, 78,154, 30, 22, 74, 60,130, 38, 83, 63, 77,123,129, 44, 12, 80,108,
129, 80,192,185,193,179, 67,123,194,192, 81,178,255, 64, 22,123, 38,129,143, 30,
154, 22, 30,  2, 38,123, 77, 68,129, 22,125, 69, 46, 81, 53, 63, 49, 12, 44, 38,
143,123, 67, 67,123,178,194,192,255,108,185,193,129,192,179,255,168, 46,  2,125,
 74,  8, 49,154,125, 22, 46, 30, 81,  2,162,125, 50, 46,  2, 74,123, 74,162,125,
  2, 76, 46, 60, 81, 63, 38, 72,129,143, 12, 73,154,125, 46,  2, 81, 22, 53, 64,
 72, 22,129, 46, 38, 30, 83, 72,129,125, 30,184, 22, 65, 38,129,125, 30, 22, 73,
115,125, 68, 38,183, 22,116, 67, 77,168, 38, 46, 30,  8,  2, 53, 62,183, 30,185,
 38, 46, 77,106, 22, 30, 46, 74, 38,130,124,194,192,185,193,187,255,128, 79, 30,
178,185,193,187,183, 76, 79,123, 78, 74, 67,129,128, 79,125, 22,184, 74,176, 83,
 22,125, 30, 38,129,143,120, 79,143, 22,129, 80, 74,154, 38, 77, 46,  2, 22, 30,
124, 79,125, 22, 74, 30, 78, 46, 18,125,182, 77, 74, 67,185, 68, 38, 77, 14, 74,
 22,125, 67,125,123,183,143,129,182, 62, 77,125, 74,183, 82,182, 83,125,129,183,
 82, 30,182,  2,183,125,182, 30, 67, 82,182, 76,123,183, 78, 79, 30, 77,128, 79,
183,176, 30,189,255, 83,176, 74,143,183,123, 30, 97, 95, 53, 88, 30, 74, 46,106,
 79,183, 78, 46, 38,176,120, 22,183,176,190,175,189, 30, 97, 46,183,185,182, 83,
178, 18,125, 80,183, 24,182, 83, 68, 82,123, 24, 38, 18, 83, 83, 79,183,185, 80,
125,182, 62,125, 83, 46, 80,183,182, 67,123,125,143,185, 79,182, 82,124,192, 75,
179,185,193,194, 26,183,125,123,143, 83, 30, 67,123, 81,185,194,192,178, 97, 46,
183,172, 54, 61, 38, 83, 46,123,183,172, 38, 81, 68, 30, 78, 22, 89, 81, 83, 63,
123, 69, 30,132,138, 77, 82, 79,121,118, 77, 79, 80, 12, 78, 66,118, 77, 79, 83,
 80, 82,115,180,118, 77, 80,187, 12, 26, 79,174,118, 77,180, 80, 67, 79, 77, 80,
 78, 12, 83, 77,121,129,123,125, 26, 32,  4,119,194, 38,123,174, 26, 78, 69, 79,
 46, 63, 22,125, 30,188, 32, 26, 44, 83, 38, 12, 70,129,125, 38, 32, 26, 12,149,
123, 32, 38, 63, 26,194, 74,115, 72, 73,129, 32,125, 71, 84, 38,180,179,194,192,
193,121, 38, 72,129,143, 12, 73,119,174, 26, 72,194, 81, 73, 69, 22, 71, 72,125,
 75, 73, 67,125,129,194, 38, 26, 32, 79,121, 44, 86,123, 38,129, 77, 65,123,129,
 72, 38, 44, 32, 16,123,129,125, 38, 32, 44,164,123, 80,174, 26, 83, 32,150,123,
 38, 81, 44, 82,125,148,123,129, 26, 77, 65,125, 80,121, 38, 77,123, 12, 44, 32,
 84, 38, 87,194,193,192,179, 64,123, 38,129,125, 78, 87, 67,123,194, 81, 32,185,
192, 69, 79,125, 83, 73,123, 81, 18,123, 44, 38,129, 87, 79, 78,121, 77,123, 44,
143, 80, 85, 84, 38,178, 85,193,194,192, 22,123, 79, 38, 77, 85,129, 16,129,123,
125, 79, 26, 44,115, 79, 77,143,180,123, 38,168, 64, 85, 79, 92, 80, 83, 65, 30,
115,123,125, 79,143, 67,129, 63,123, 38, 36, 79, 80,125,121,123, 79,125, 74, 80,
 36, 69,125, 79, 24, 83, 36, 42, 64,123, 79, 36, 22, 38,174,  2, 60,129, 79,123,
193,194, 38,121, 77, 74,123, 79,129,125, 79,129,125, 80, 82,123,174,115,129, 77,
 30,180, 79,123,119,123, 74,125, 77, 78, 80, 86, 80,123,125, 77,129, 79, 67,123,
125, 74, 30, 22, 46,125,121,130, 30, 74, 53,116, 79,115,180,116, 79, 30, 46, 67,
 86, 38,178,194,192,193,255, 72, 79, 77, 80, 74, 60, 83,168, 30,  8, 38, 46,116,
130,148, 38, 53, 74,130, 22, 30,123,115, 30,136, 46, 38, 80,128, 79, 30,136, 80,
128, 83, 82, 69, 30,132,138, 77, 82, 79,121, 30, 38, 77, 80, 79, 22, 18, 83, 79,
 38, 30,136, 80, 72, 30, 79, 46, 38,180,136, 79,121,123,184,125, 22,129, 80,188,
 80, 82,184,185, 22, 78,119,194, 72, 93,143, 78, 77, 72,123,129,192,125,184,158,
 18,123,194, 86, 72, 83,184,115,129,125, 72,184, 22,143, 74, 86,194, 38,178,180,
192,193,115, 72, 53, 38, 46,123, 75,121, 38,129,125, 30, 22, 73, 63, 32, 28, 22,
 72, 40, 24,119, 38,194, 46,129, 72,143, 69, 22, 71, 72,125, 75, 73, 67,125,101,
 30,130, 46, 22, 38,192,148,130,192, 38, 74, 22,138,168, 30,  8, 46, 38,116,130,
 65, 30, 38, 46, 74, 79, 53, 74,154,116,180, 38, 30, 22, 69, 22, 30, 46, 74, 38,
130,123,121, 30, 38, 22, 80, 78, 79, 69, 30,132,138, 77, 82, 79,115,182, 30, 22,
 82, 80, 77, 63, 77, 30, 12, 79, 34, 80, 22, 22,128, 83, 82, 79, 77, 65, 38,136,
 30, 78, 77, 79, 30,115,123,125,143,185, 79,182, 63,123, 38, 36, 79, 80,125,121,
123, 74,192,184,143,125, 69,125, 79, 24, 83, 36, 42, 16,123,129,194,125,193, 74,
 64,123, 79, 36, 22, 38,174, 38,115,125,123,143, 30,129, 74,121,123, 74,125,143,
 44, 30, 74, 32,125,129,180, 46, 30,  4,129,123,125,193, 74, 30, 16,125,194,123,
193, 74, 30,119,194,123,125, 22, 30, 14, 22,115,123,183,125,182, 30,129,121,123,
184, 74,143,182, 30,168, 30, 16, 28,129, 10, 38, 16,194,123,184,129,182, 74, 74,
125,129, 28,180,143, 46,119,194,123, 38, 30,125, 14, 46,115,125,123,183,143,129,
182,121, 74,123,184,125,143, 30, 16,194,129,125,123, 74, 30, 63, 38,129, 79,194,
123, 77,168, 30,123,129,143, 38,125,148,123,194,129, 30, 22, 14,119, 78,188,184,
 46, 30, 82, 64,  8,128, 79, 30,178,193,185,179, 22, 81,125, 22,129, 83, 77, 64,
 71,129,123,125, 30, 46,164, 80, 64, 83, 79, 82, 57, 67,123, 81,185,194,178,192,
125, 64, 38, 82, 74,116, 78, 46, 63, 82,116, 12, 78, 77, 26,106, 22, 30, 46, 74,
 38,130, 65, 38, 78,116, 30, 67, 77, 68,116, 30, 38, 78, 82, 77, 66,116, 38, 82,
 80, 77, 78, 77,188, 46,184, 22, 38, 81,192,128, 79, 30,178,193,185,179, 67,194,
123, 81,185,178,192, 64,129,123, 78, 83,125, 22, 65,194, 30,125,123, 38,129, 83,
 82, 78, 83, 81,123, 30, 30,106,125, 79, 24, 83, 36, 42, 18,184, 83, 82,182,178,
194,114,125, 46, 74, 79, 80, 24, 64,123,174, 36, 74, 38, 67, 22, 82,125,194, 83,
192, 60, 26,123,194,178,125,192, 60, 83,128, 30,178,194,193,185,179, 63, 78, 80,
 12, 38, 82, 77, 83,123,129,143,194, 38, 76, 64, 22,123, 38, 78,143,125, 16, 82,
125,129, 30,123, 81, 18, 30,194, 38, 78,125,123, 82, 67,123, 81,192,178,185,194,
128, 30, 79,178,179,185,193, 26, 78, 81, 30, 46,123,125,164, 46, 80, 22, 68, 38,
 30, 63,125,123, 80, 78, 12, 26,106, 79, 75, 83, 81,123, 68, 78,115, 77, 83, 56,
129,125, 20,123,143, 62,125, 32, 20, 56, 26, 46, 97, 56,125, 46,129,123,143,150,
125, 49, 32, 46, 84, 56, 67,192,123, 32, 81,194,180, 22,125, 32, 56,123, 22,129,
129, 16,116,120,138, 46, 26, 32,128, 30, 79,194,179,193,187, 22,116, 32, 77,187,
 79, 22,124,194,192, 26,124,193,179, 90,180,138,192,194,193,255,  4,116,120,187,
 79, 32, 77, 32,124,185, 26,193,192,194,255,128, 79,178,185,193,194,255, 16, 81,
 77, 71,184,170, 38,150, 81, 77,182, 71, 38,183, 22, 77, 81,125,183,129,194, 18,
 57, 77, 71, 81, 85,129,123, 83,187,136, 26, 83, 77, 82, 28, 83, 81,136, 26, 22,
 82, 16, 83,136, 71,138, 82, 77, 62, 83, 20, 77,136, 82,118, 22, 77, 81, 83,132,
 82, 30, 26, 83, 32,136, 26,132, 14,143, 83, 32, 79,187, 77, 22,172,  4, 79, 77,
 32, 26, 83, 82, 22, 79, 22, 77, 82, 83,128, 55, 20, 79, 32,187, 85, 77, 16, 79,
138, 85, 71, 83, 82, 62, 79, 20, 77, 71, 26, 83, 46, 67,123,192,185,194, 81,180,
124,192,185,179,194,255,255, 26,183,182,125,123, 71, 38, 83,129,183, 82,125,182,
 30, 68, 14,129, 77, 71, 22,125,  4,125, 77,183, 40,129,123, 16,125,148, 81, 82,
 80, 32,138, 83,168, 81, 71, 83, 79, 92, 64, 22, 32, 81, 79, 77, 71, 22, 24, 32,
 81, 71, 30, 79,192, 10, 32, 30,154,192, 79, 80, 68, 81,116, 32, 82, 71, 30, 81,
115,125, 82, 74, 83,170, 67, 67,125,194,123,193, 74, 30, 62,125, 60, 22, 74,192,
 30, 10,125, 32, 30, 83, 14, 78, 83, 60,125, 82, 78,143, 79, 24, 22,125, 30, 60,
 46,178, 82,115,125, 81, 68, 77, 75, 54,121, 32, 83,125, 30, 75,123, 67,123, 32,
 81,192,178,194, 68, 32, 81, 75,123, 83,125,119, 30,125, 75, 22, 14, 46, 65, 61,
123,129, 81,125, 54, 83,115, 81, 32,123, 76, 26, 77,119, 30, 22, 26, 76,129,123,
 24,154, 81,178,125,192, 79, 10,123, 76, 30, 22, 26, 14,  8, 30, 81, 76,125, 82,
 79, 67,123,194, 32,178,192, 81, 77, 68, 81, 83, 32,129, 82,125,119, 83, 82,125,
 22, 14, 26, 24, 81,125,194, 79, 83, 32, 67,123,194,192, 32,178, 81, 83, 83, 82,
 79, 78, 22, 46,148,123, 81, 82, 26, 78, 83, 32,115, 81, 77, 71,184,170, 38, 67,
 81,184,125,143,182, 77,148, 81, 80, 71, 26,129, 79, 66, 81,123,129, 79,125, 24,
 83, 81,129,184,125, 38, 24, 24,125, 79, 71, 92, 85, 81, 67,123,121, 71, 79, 77,
 20, 81, 80,115, 71, 77,193, 32, 20, 26, 60,136, 30, 71, 64,188, 79, 16, 79, 82,
 77, 83,193, 26,119, 30, 71, 82,193, 80, 83,  4, 71, 30, 79, 64, 26, 81, 32, 16,
 81,184,125,143,182, 77,121, 57,184, 40,125,143, 77, 65,129, 79, 85,143, 38,125,
 22,125, 79, 81, 71, 77, 40, 63,129, 85,125, 77, 71,143, 64,129,123, 81,125, 24,
 71, 81,115,125,123,143, 30,129, 74,121,123, 74,125,143, 44, 30, 74, 32,125,129,
180, 46, 30,  4,129,123,125,193, 74, 30, 16,125,194,123,193, 74, 30,119,194,123,
125, 22, 30, 14,192,121, 79,184,190,129, 14, 20, 65, 79,190, 30, 92, 26,174, 63,
 44, 79,190, 77, 30,174, 64,129, 79, 30, 32, 14, 22, 81, 79,190, 80,129,174, 71,
148,129, 32, 71,125, 85,194,194,121,190,191,192,189, 20, 14, 63, 79, 44, 71, 46,
 77,190,164, 80,192,190, 64, 85, 77,148, 81,192, 30, 46, 79, 80, 64, 30,123, 79,
 22, 71, 44, 65, 79, 46,125, 77, 71,190,178, 18,179,255,255,255,255,255, 81,179,
171,255,255,255,255,121,184,190, 30, 44, 22, 20, 66,179,172,255,255,255,255, 22,
 79, 22,190, 77, 20, 30, 16,194,184, 71,123,125,190,121, 22,108, 80,192,129,185,
193,179, 83, 82,123, 28,143,184,182, 67,123,194, 81,178,192,255, 63, 77, 14, 28,
 30,123, 85, 18, 71,125, 80, 82, 83,184, 62, 71,184, 16,125, 80, 79,125,108, 80,
192,193,179,187,255,154, 77, 32, 79, 83, 71, 22,149, 81, 14, 79, 32, 26, 77, 65,
 32,116, 71, 81, 80, 20, 66, 71, 77, 79,188, 81, 85, 28, 71, 81, 80, 32, 26,116,
 26,108, 80,192,129,185,179,193, 63, 77, 44,123,129,143, 38, 67,123, 81, 32,192,
 18,194, 83, 82, 81, 79,129,184, 71,168, 80, 79,180, 82, 81, 83, 26, 71,125, 80,
 28, 81, 22, 32,108,185, 80,192,193,129,255,149,  8, 71,125, 40, 77, 24, 67, 57,
184, 40,125,143, 77, 63, 57, 77, 24, 44,129,143, 83, 57,125,184, 26, 20,  0, 64,
129,125, 71, 24, 81,  0, 71, 83, 72, 20, 26, 44,125, 30,108, 72,129,179,192,193,
255, 67,123,192, 32,194,178,255,149, 72, 32,125, 44,143, 46, 90, 32, 44, 26, 30,
 46, 72, 65,123,129, 72, 32, 46, 44, 46, 83, 82,184,123, 79,182,125,108,129, 80,
185,192,179,255, 67,123,192, 81,194,178,255, 63, 71, 79, 77, 30,123, 80, 62,123,
 40,182, 80, 22,125, 90, 79, 77, 71, 40, 81, 80,148,123, 90,178,138,180,193,179,
194,152, 83, 82, 81,192,178,136, 67, 64, 83, 77, 79, 20, 82,153, 30, 83, 79, 81,
132, 22,151, 81, 83, 71, 26, 82, 22, 22, 81, 82, 80, 83, 30, 46, 83,115, 76, 82,
 14, 78, 80, 30,119,194, 81, 78, 26, 30, 14, 22,123, 81, 26, 32, 76, 82, 67,123,
 32,178, 81,194,192, 83, 55,123, 32,194,129, 30,  4, 81,123, 80,194,125, 82, 80,
115, 32, 82, 78, 73, 87, 83,119, 81, 78, 83, 77, 32,125,121, 78, 79, 87, 32, 73,
123, 97, 46,129,123, 20, 32, 44, 67,123,194, 32, 81,192,255,101, 87, 78, 83, 73,
 32, 81, 79,115, 78, 82, 32, 86, 80, 26, 16,123, 81, 82, 26, 32, 78, 62,123, 83,
 78, 81, 32, 82, 97,123, 46, 44, 30, 93, 83, 67,123,194, 32, 81,178,192, 22, 81,
 26, 65, 82, 83, 44, 32, 65,129,192,125,143, 71, 64, 22, 81, 80,125, 26, 79,129,
 16, 81, 80, 71, 26,129, 79, 63,  8,129, 71, 85, 57, 64, 67,129,192, 57, 71,194,
 81,151,125, 80, 79, 77, 40, 81, 81,115,125, 53, 14, 74, 82, 44, 67,123,194, 32,
 74,129,125,  2,123, 32, 78,125, 77, 14,119, 78, 14, 46, 30, 44, 83, 83,123, 14,
 46, 80, 78, 82, 28,123, 22, 30, 46,125, 32,119,125, 64,194, 79,116, 81, 71, 30,
 65,194, 81, 80, 71, 82, 77, 18, 14, 32, 81, 30, 71, 26, 22, 81, 71, 82, 32,194,
 77, 34,179, 14, 82, 30, 81, 80, 66,180,188, 82, 77, 30, 80,123, 90,180,138,193,
178,194,179,128, 79,180, 30,178,193,128, 67, 30, 71, 82,193, 80, 83, 22, 82, 80,
 83,136, 81, 77, 16,136, 83, 80, 30,132,138,106, 71, 77, 79, 83,136, 85, 14,188,
184, 46, 30, 82, 64,  8,128, 79, 30,178,193,185,179, 22, 81,125, 22,129, 83, 77,
 64, 71,129,123,125, 30, 46,164, 80, 64, 83, 79, 82, 57, 67,123, 81,185,194,178,
192, 32, 65,194,129, 38,125, 81, 85,128, 79,178,185,193,194,255, 64,129, 81,194,
 24,174,123, 18, 57, 81,125, 71, 85,  0, 22, 81,125,129,194, 24, 38, 28, 81,194,
182,125, 57, 40, 82, 76, 46, 89, 30, 78, 20, 14,128, 30, 79,178,194,179,193, 97,
 30,123, 89, 46,125, 14, 55, 30, 89,123,125, 14, 46, 66,194, 75,125, 89, 81, 77,
 62, 89,123,125, 20, 30, 81,129,128, 30, 79,194,179,193,187, 16,120,116, 46,138,
 32,192, 22, 26,116, 46, 32, 77, 44, 90,194,138,192,179,193,255, 28, 26, 22, 14,
116, 46, 32, 18, 26, 32,120, 46, 22, 44,

121, /* b2-c4 */
 77, 69, 56,162, 63, 59, 91, 77, 84, 49,164, 63,125, 70, 77, 57,192,106,123,125,
 63,129, 14, 32,163, 63,123, 32,129, 26, 70,149, 14, 32, 22, 30, 70, 26,148, 77,
 59, 70, 91, 84, 63,125, 18, 56, 22, 26, 46, 30, 79, 63,116, 32, 30, 22, 80, 20,
108, 80,192,193,179,187,255, 65, 32,116,138, 30, 26, 22,  2, 63, 32, 56, 81, 44,
 78, 66,188,116, 63, 56, 84, 91, 26, 64, 56, 38,129, 84,123, 44,108,185, 80,129,
193,179,192, 83,129, 56, 79, 82, 20, 81, 67,123, 81, 32,194, 18,192,106,194,129,
125, 79, 32, 83, 62,184,125, 56,123,182, 34, 32,108,185, 80,193,129,192,255, 83,
 56,125,  0, 20,182, 26, 67, 56,182, 40,184,125,143, 63,129,125,  0, 24, 26, 20,
 64, 78, 56, 38, 44, 84, 40,106, 78,125, 79, 24, 63,194, 22,108,185, 80,129,193,
192,179, 67,123,194,192, 81,178,255, 83, 82,184,123,143,125, 79, 62,184,182, 63,
 14, 78, 81, 97, 46,184, 79,129, 28, 80, 90, 46, 10, 28, 82, 79, 81, 46, 67,123,
 81,194,192,178,255, 64, 22, 38, 56, 79,123, 63, 83, 56, 82,125,123, 79,184, 63,
129, 79, 22,184, 38,125, 62, 56,182,184, 40,123, 14, 90, 40, 79, 56, 81, 80, 78,
 67,123,108,193,180, 80,179,187,192, 60, 20, 26, 44, 63, 80, 56,  2, 63, 81, 20,
 78, 83, 79,164, 80,188, 63, 26, 30, 81, 22, 63, 26, 44, 20, 80, 81,  4, 30, 63,
 20, 26, 56, 22, 32, 81, 81,192,194,255,255,255,108,129,185, 80,193,192,255,164,
 80, 63,125,143,184, 81, 63,129,125,  0, 24, 26, 20, 69, 56,125,129, 79, 24,  0,
 64, 78, 56, 38, 44, 84, 40,194,136,195,255,255,255,255,255,108, 80,195,129,255,
255,255,130, 81,195,193,255,255,255, 63,190, 32, 26,191,193, 44, 81, 56,190, 14,
191, 22,189, 65,190, 79,191,189, 81,193, 81, 81,194,192,255,255,255,255, 74, 44,
 32, 30,192, 14, 22,108,129,192,193,255,255,255, 18, 80, 32, 22, 14,129,123, 69,
125, 44, 22, 46, 80, 74, 28, 74,123, 77, 44,174,193,178,116,179,255,255,255,255,
255, 18,184,255,255,255,255,255, 81,184,171,255,255,255,255, 65, 79,190, 30, 56,
194,176,134,171,255,255,255,255,255, 22,184, 56,190,176, 30, 22,192,134,255,255,
255,255,255,255, 81,190, 56, 30,184, 44,189, 65, 79,190,174, 56,176,189, 22,184,
 79,190, 56,176, 84, 16,129,184,176, 56, 63,190, 74, 79,184, 56,190, 30, 44,188,
 83, 34, 32, 73, 60, 30, 46, 87,181,123, 32,129, 44, 46,193,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255, 32,108, 80,193,185,192,255,255,160,129, 79, 80, 81, 78, 56,148,129,
 79, 78, 81,143, 63,164, 80,129, 79, 38, 40,125,153, 24,123,129,143, 44, 84,130,
 80, 31, 84, 49, 70, 56, 81,148,123,129, 78, 53,125, 80,108,193, 80,192,255,255,
255, 51, 74, 83, 32, 67, 60, 30,160, 83,123,129, 22, 14, 74, 58, 80, 83, 74, 32,
 67, 53,126, 32, 46, 82, 74, 80, 67, 26, 67, 81, 18, 32,192,178,255,108, 80,193,
192,185,179,255,126, 32, 74, 44, 79, 38, 65,148,123,129, 81, 78, 32, 79,160, 83,
 81, 32,143,123,129,153, 83, 49,123, 81, 18, 44, 44, 67, 32, 81,192,178,255,255,
108, 80,193,185,192,179,255, 51, 83, 32, 26, 80, 63, 81,  2, 32, 83, 81, 63, 38,
 26,148,123, 81, 78, 32, 79,182,160, 83, 81, 32, 80,143,123, 79, 67,192, 32, 81,
178,255,255,108,193, 72, 80,192,179,255,148,123, 78, 32, 80, 81, 44,  2, 32, 83,
 81, 26, 78, 77, 51, 83, 32, 81, 44, 78, 26,153, 83,123, 81, 82, 77,125,108,129,
117, 22, 32, 26, 78, 84, 63, 69, 46, 56, 79, 44, 30, 26,113,193, 20, 26, 44, 22,
 56, 64,187,192, 79, 22,193, 70, 66,193,187, 56,188, 80, 79, 63,187,192, 32, 79,
 22, 46,193,121,123, 79,190, 32, 56,189, 29, 30,129,190, 91, 56,125, 67,123, 32,
 81,192,255,255, 64, 32,129, 22, 44,123, 56, 31, 30, 80, 20, 79, 22, 91,123, 56,
189, 20, 26,129, 80, 80,113,123,125, 87, 20, 26, 30,117,123,125, 30, 26, 20, 22,
 67, 32,123,194,192,255,255,123,123, 26, 32, 20, 44,129, 69,125, 26, 22, 46, 30,
 14, 63,125, 22, 44,129, 30,143,192, 29,193, 78,255,255,255,255,123,189,180,184,
 56, 79,191,121, 79,190,174, 56,176,189,188, 30,193, 22, 46, 14,129,113, 56,189,
195, 14, 22,184, 63,129,125, 32, 22,193, 44,179,121,171,178,255,255,255,255, 63,
193,171,143,177,123,125, 67,171,123,173,255,255,255, 72,178,255,255,255,255,255,
113, 56,123,129, 30,173, 14, 64, 56,193,123, 30,129,143,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, 63,129, 70,
116, 22, 79, 30,158, 46,108,187,192, 32, 79, 22, 46, 69, 32, 46, 56,116, 26, 14,
150, 63,114, 32, 26, 78, 22,149, 32, 63,124,134, 22, 20, 64, 56, 70, 22,120, 78,
 79,123,108,193,180, 80,179,187,192, 22,136, 80, 83, 82, 22, 30,  4,136, 80, 79,
 82, 81, 78,149, 26, 32, 70, 22, 20, 30, 26,136, 80, 83, 30, 78, 22,112, 91, 79,
 63, 81, 78,136,125, 18,130, 82, 79, 80, 20, 22,108,192,187, 80,193,179,255,149,
130,188, 26, 32, 79, 20, 69, 32, 56, 79, 80, 81, 22, 65, 32,116,138, 30, 26, 22,
 34, 32,138, 46, 22, 30,116, 79, 65, 77,123, 32, 86, 44,129, 68,123,129, 86,178,
 32,125, 67,123,192, 32,194,178, 81, 66,123,129,194,125, 80, 86, 26,174, 86,123,
129,125, 30, 64,123,129, 86,125, 80, 93, 26, 64, 56, 38,129, 84,123, 44, 18,129,
143,125,184, 34, 44, 77, 56,129, 20, 49,125, 44, 70,129, 38,125, 44,143, 32,112,
 38,129,125,123, 44, 79, 69, 56,125,184,123, 34, 20, 32, 70,129, 38,  0,125, 24,
 26, 67, 56,182, 40,184,125,143,108,129,192,185,193, 80,255, 18,129, 24,125, 20,
 26, 44, 34,  0,129,125, 38, 40,143,  2,129,125, 38, 40,143, 24,150, 49, 51,129,
 26, 14,143, 32, 44,  2, 30,123, 91, 77, 56, 63,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
123,158, 83, 81,188, 56, 22, 80,108, 80,193,180,179,192,187,166, 82,136, 79,132,
 78, 83,152, 82,136, 79, 20, 80, 83,116, 30, 49, 83,136, 79, 26, 18, 80, 78,136,
188, 32,180,129,166, 63, 26,124, 32,134, 22, 63, 63,114, 32, 26, 78, 22,152, 49,
 63, 91, 32,138,116,149, 49, 63, 32, 14,124,134, 51, 22,114, 30, 63, 91, 26,153,
 49, 30,116, 32,142, 63, 80, 67,123,192, 32, 81,180,194,108,123,125, 30, 32, 26,
 87,149,123, 14, 81, 66, 26, 32, 68,123, 81, 32, 87, 20, 77, 66,123,129, 32, 44,
 20, 87, 69,129, 32,125, 79, 87, 44, 81, 18, 80,123,129, 67, 32, 44,148,123,129,
 78, 53,125, 80,108,193,129, 80,180,192,255,162,123, 32,193, 30, 78,143,166,123,
 82, 74, 22, 30, 14,168,123, 32,143,129, 44, 80, 32,108,193, 80,129,185,180,192,
158,129,125, 78, 79, 81, 80, 51,123,129, 24,  8, 79, 81,106,123, 79,129, 38,125,
143,112,123,129,125, 79,143, 44, 18, 49,123,129, 79, 80,125, 74, 18,129,116, 26,
 44, 20, 73, 46, 22,162, 67, 26, 44,124, 46,142,149, 30,134, 72,188, 73, 26,155,
 32, 22, 30, 72,120,124, 72, 30, 73,180, 81,120,188, 10, 72, 30, 26,116, 44, 22,
 73,158,129,125, 44, 26, 20,123,154,123, 44, 59,125, 26, 22,116,129, 72, 80,179,
192,193, 66,123,129, 22, 14, 75, 87,155, 66,123,193, 26, 59, 20,162,123, 26,125,
 20, 44,129, 75,162, 44, 32,125,129,143, 73, 66,129, 61, 22, 46, 76, 82, 67,123,
 74,192, 32,194,178,116, 72,129,179,192,193,255,148, 54, 44,143,129,125, 73,155,
180, 44, 32, 61, 73, 82, 72,132,192,178,179,193,255,255,114, 32, 74,194,192,180,
255,128,194, 79, 30,178,179,193, 24,129,123, 44, 32,125, 79, 67,123, 74, 30,192,
 26, 79,160,129,123, 73, 30, 32,125, 22,116,129, 72,179,185,192,193,162,129, 46,
 38, 30, 28, 67,148,129, 46, 72, 38, 14, 30,155,129, 38, 46, 28, 30, 72,154,129,
 67, 72, 73, 46, 30,150, 30,129, 73, 46,125, 38, 44,116,129, 72,179,185,192,193,
148, 72,129, 36, 81, 73, 75,108, 72, 26, 32,125,129, 20,158,129,125, 75, 73, 36,
 20,149, 72, 26, 73, 36,125, 20,162,129, 73, 75, 36, 20, 76,162,143,108,193,180,
187,179, 72,255, 69,128, 67, 76, 53, 73, 26, 68,128, 26, 67,138, 81, 73,154,134,
130,138, 44, 20, 32, 67,130,128, 32,138, 30, 44,148,128,138, 53, 44, 30, 26,129,
 69, 32, 67, 46, 76, 75, 26, 16, 67, 53, 81, 32, 44, 46, 67, 26, 46,138, 44, 20,
 22, 18, 67, 26, 44,124, 46,142, 26, 67,116, 73,138, 32, 81,108, 26, 22, 75, 53,
 73, 44,123, 18,128,194,193,108, 82,136,154, 82, 61, 96, 68, 90, 32, 67, 32,138,
 68,108, 75, 26,108,193,180,179,192,255,255, 16, 68,194, 20,128,136, 32,183, 54,
193, 82, 90,108,136, 32,108,129,193, 72,185,180,192, 69, 67,125, 76, 81, 40,129,
 18, 67, 40,125, 20, 75, 26, 66, 73,129, 24, 67,123,143, 67,182,123, 40,125, 26,
 67,148, 53, 81, 24, 72, 26, 67, 26, 66,129,123, 67, 73, 20, 32,108,129,180,179,
193,185, 72, 67, 32,129,125, 20, 38,143, 16, 73,129, 44, 20,125, 60, 68,129, 81,
 32,182,143, 53, 69,125, 10, 75, 67, 73, 34, 67,108,180,129,192,179,193,255, 66,
129, 32, 26,193, 30, 44,190, 20,123, 26,180, 14, 30,154, 66,123, 26, 74, 20, 44,
 18,123,125, 80,143, 73,174,168,123, 66, 74, 32,129, 69, 67,129,162, 26, 46,138,
 44, 20, 22,116, 73, 75, 76, 46, 81, 30,148,114, 26, 22, 20, 44, 72,155, 32, 26,
124, 20, 44, 22,154,120,114, 32, 26, 44,134, 69, 72, 76, 46,116, 26, 44, 72, 16,
123, 74, 30,192, 26, 79,114,123, 32,192, 74,194,255,162,123, 74,178, 26, 32, 30,
 65,129,125, 26, 74, 32,123,150,123,129, 71, 32, 73, 46,148, 71,123, 32, 44, 30,
 79, 32, 18, 72, 73, 81, 20, 44, 26, 63, 81, 38, 72, 24, 44,174, 22, 72, 26, 24,
184, 20,  0, 28, 72, 24, 67,  0, 44, 20, 65, 81, 24, 26, 72, 38, 44,154,129,125,
123, 40,143,192, 26, 74,180, 32,194,178,192,255,162, 32,129,125, 20, 38,143,154,
129, 72, 32,143,125, 75,168, 72,180, 32,129, 75,125,116,194,129,125, 38, 32, 75,
 28, 72,129,180, 32,194,143, 20, 74, 32,180,194,178,192,255,162, 32, 26,125,129,
 38, 44,116,129, 38,125, 32, 76, 26,154,129, 32,143,125,123, 81, 63, 38, 72,129,
143, 12, 73, 28, 72,129, 32, 26, 44,  4, 44, 74,180, 32,192,194,178,255,162,182,
 32, 26, 20,125,129, 64, 73, 36,129, 72, 38,143,154,129, 32,143,125,184, 20, 28,
 72,129, 32, 28,125,184, 69, 72, 32,125, 26,129, 36, 69, 72, 16,125, 74, 32,123,
 26, 22,114,125, 22, 32, 76,129, 44, 65,129,125, 26, 74, 32,123,164,123, 71, 79,
 86, 93, 70,148, 71,123, 73, 30, 79, 32,155, 71,123, 32,172,125, 14, 32,162, 67,
125, 76, 81, 40,129,154,129,125, 81, 73, 76, 40, 63, 81, 38, 72, 24, 44,174, 67,
 72,184,  0, 20, 38, 24, 65, 81, 24, 26, 72, 38, 44, 83, 81, 72,184, 20, 26, 67,
125,154, 32, 76, 44, 26, 30, 75,162, 76, 32, 53, 26, 67, 44,108,179,193,180, 72,
187,192, 28, 72, 26, 73, 81, 75, 44, 65,116, 72,138, 73, 30, 26, 18, 72, 67, 26,
 73, 30, 75, 26,154,125, 73, 75,129, 81, 34,162,125, 10, 75, 67, 73, 34, 67, 72,
129, 32,125,184, 20,108, 72,129,185,193,180,179, 26, 28, 18, 72, 60, 22,  4, 63,
 75, 38, 72,129,143, 44, 30,154,125, 76, 73, 46, 75, 24,162, 73,125, 76, 67, 81,
 24, 63, 72, 36, 38, 46,129,143, 64, 72, 22, 36,129, 46, 42,108, 72,129,185,193,
179,180, 83, 72,129,143,125, 42,  6, 22,162, 10, 72, 14,125, 46, 28, 63, 38, 72,
 46, 28,143,129, 67, 72,184,129, 38, 46,125, 83, 72,184,125, 30,129,143,154, 73,
125, 76, 16, 75,129, 55, 72,125, 60, 73, 67,184,148, 32,154,129, 53, 81, 73,125,
 44,106, 38, 81, 26, 24, 53, 20,108, 72,185,193,180,192,129, 18, 67, 81, 72, 24,
 73, 44,155,129, 75, 76,123, 24, 73,162,123, 53,129, 73, 24, 67, 53, 51,123, 67,
129, 32, 81,143,154, 32,129, 51, 26, 20, 52,155,123,129, 32,125,143, 74,  2,123,
 81, 30,129, 67, 60,  4, 52,123, 32, 60, 80, 73,255,255,255,255,255,255,255,129,
190,116,142, 26, 46, 44, 53, 67,114, 26, 22, 20, 44, 72, 16, 53, 30, 73, 67, 81,
 26,162,116, 32, 67, 26, 44, 73,155, 72, 32,116, 53,124, 30, 51, 30, 26, 67, 81,
116, 22,123,154, 82, 61, 96, 90,136, 75,183, 54,193, 82, 90,108,136,155, 82, 54,
 75, 30, 61, 90,162, 54,128, 82, 30, 68, 32,176, 82,136, 54, 32, 44, 75,108,193,
179,180,192,255,255, 26,162, 10,129,143,123, 53, 67,108,129,180,193,179,192,185,
176,129, 81, 32, 18, 44,125,190, 75, 32,123,129, 72, 44,147,129, 18, 32,123, 20,
 44, 51,129, 32, 72, 75, 67, 81, 72,114, 74,194,192, 32,180,255,132,192,178,193,
179,255,255,108, 71, 32,123,193,129, 79,164,123, 74, 79, 86, 71, 93,128,194,193,
 30, 79,178,179, 34, 71,123, 32, 30, 74, 86,  2,129,162, 32, 67,116, 20, 26, 46,
155, 72, 32, 76, 30,120, 46, 10, 72, 30, 26,116, 44, 22, 18, 67, 73,134, 22, 26,
120, 26, 73,120, 30, 44, 26, 46,  8, 73, 72, 67, 44,116, 26, 30, 18, 18,129, 67,
 72, 24, 75, 64, 72, 22, 36, 38, 46, 12, 63, 36, 72, 38,143,129, 46, 10,129, 24,
 38,125, 72, 22,154,129,123, 18, 60,194,125,108, 72, 81, 22,194, 73,184, 22,162,
 30, 28, 38,129, 46,193, 18,129, 67, 73, 72, 46, 28,148,129, 30, 38, 28, 14, 46,
 67, 72,184, 38,143,125,129,  8,129, 38,125, 46, 73, 53,168, 46, 38, 30,129, 16,
193, 32, 18, 72, 81, 24, 44, 20,180, 10, 81, 24, 26, 72, 38, 44, 66,129, 81, 24,
 60, 72,123, 14, 81, 26, 24, 44, 38, 60, 34, 81, 67, 26,194,180, 38,  8, 81, 76,
 26, 38, 24, 60,123, 10, 61,138,136, 68, 82, 54, 26, 90,136, 96,138, 82, 30, 67,
 61, 82, 20, 44,138, 68, 68,136, 75, 30, 82,138, 90, 66, 61, 82, 96, 68, 90,193,
 18, 61,136, 82,138, 30,180, 67, 66,123, 81,125,143, 60, 53, 10,129, 74,143, 68,
 81,123,154, 66,123, 32, 60, 30, 44,148, 81,123, 53, 32, 60, 74, 14,123,129, 26,
125,143, 74,162,123, 66, 32,129,143,125,123,108, 80, 66,136, 22, 26, 30, 32, 44,
117,193,136, 30,188,174,180,123,121, 26, 30,180, 20, 22,113,179,193,136, 30,180,
 26, 99,136, 30,180,179, 20, 46, 17,128, 77, 79,136,194,110,193,123,117,103, 32,
 80,121, 78, 31,128, 30,136, 44,181, 14, 63, 77, 44,136,191,128, 80, 69,118,132,
191,190, 83,136, 64,136, 22, 77, 44, 78, 80, 68, 88,138,191, 82,190, 80,192, 29,
193, 78,255,255,255,255, 17,195,194, 79, 30,174, 44, 72, 30,180,118,132,128,110,
117,136,193,195,174,191,184, 11, 30,194,136,195, 44,190, 31, 30,136,176, 22, 44,
174,180, 68,188,181,174,255,255,255,117, 80,136,173, 79, 95, 88, 67, 77, 80, 79,
136, 88, 95, 69, 77, 80, 79,118, 95, 32, 17,136, 80,181,173, 32, 74, 29,136, 78,
 79, 95, 32, 88,187,123, 74,188,180,193,195,255, 68,188,181, 82,195,193,255, 69,
136,118,173,180, 22,132, 64,136,118, 22, 44, 14,173, 63,136,118, 22, 95, 44,173,
 31,136,118,128, 14,110, 22,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255, 18,136,149, 17, 77,121, 82, 79, 74, 66,
 31,127,131,121, 80,145,157,121, 80,  1, 41, 33, 77,150, 19, 83, 80, 37, 77, 78,
162, 78, 79, 19, 80, 77, 82,155, 78,121, 17, 83, 80, 77, 80, 66, 73, 82, 83, 79,
136, 30, 63, 59, 73, 79,136, 82,188,116,188,136,187, 30, 20, 44, 10,136, 73, 30,
 77, 20, 26, 26, 30, 73, 83,136, 59, 77, 24,136, 82, 83, 77, 81, 78, 83,116,193,
 80,187,179,192,255, 10,136, 62, 77, 79, 80, 26, 66, 62, 82, 79,193,188,187, 63,
 62,136, 80, 79,188, 82,108, 77, 62, 78, 55, 80, 79, 12, 77, 30, 55, 82, 80, 62,
 79,149, 26, 93,136, 83,188, 80, 66, 83,194, 82, 26,192,193, 10,136, 77, 20, 80,
 26, 30, 24,136, 82, 83, 77, 20, 30, 12,136, 77, 30, 80, 83, 82,116, 72, 80,187,
193,179,192,188, 30,136, 83, 46, 44, 95, 80, 24,136, 83,181, 30, 44, 77, 67,136,
181,195, 77, 83, 44, 36,136, 77,195, 83,181, 30, 26,136, 83,187, 30,180, 22, 10,
136, 77, 79, 83, 30, 22, 44, 63, 38, 28, 79, 80, 83, 36,116, 80,185,193,187,192,
255, 64, 38, 36, 78, 79,136, 80, 66, 79, 80, 32, 38,188,187, 10, 77, 80, 38, 79,
 36, 83, 24, 38,136, 82, 83, 77,132, 69,136,149, 83,121, 82, 74, 41, 17,162, 83,
 78,121,  1, 82, 74, 66, 80,127,121,131,145,141,163,123, 83,121, 33,  1, 74,155,
121, 78, 83, 74, 17, 41,150,170, 19,121, 83, 82, 17, 46,108, 80,192,180,187,185,
255, 66, 38, 80, 22,188, 14, 79, 63, 38, 30, 80, 79, 78, 77, 64, 38, 22, 78, 79,
 14,136, 83,184, 78, 79, 77, 82,182, 97, 79,138,184,187, 34,182, 79, 18,136, 86,
 26, 44,138,132, 28,136, 77,180, 80, 26, 44, 83,136, 20, 44, 80, 46, 26, 22, 83,
 80, 30, 77, 81,136, 67, 20,193, 86, 77, 26, 30,154, 83, 77, 32, 46,132, 26, 30,
108, 80,192,180,193,187,185,154, 83, 79,132,114, 46, 80, 66, 80, 83,128, 38, 22,
188, 63,128, 38, 80, 83,136, 24, 67,193, 80, 38, 36,187, 24,155, 24, 83, 79,132,
 14,138, 44,108, 80,193,192,180,187,185, 90,138,180,194,192,193,178, 18,184,132,
 79,187, 80,138, 97, 79, 80,187, 28,184,138, 28, 79, 80,132,138,118, 28, 26, 38,
138, 77,184, 80,132, 80, 90,138,180,194,193,192,255, 83,136, 79, 44, 26, 20, 30,
 55,136, 83, 20, 87, 26, 22, 62,136, 83, 20,132,118, 87, 67, 79, 26, 77, 73, 87,
 30, 66, 73,136, 30, 77, 79, 20, 67, 30,108,193, 80,192,180,185,187, 66, 80, 83,
128, 38, 22,188, 63,128, 38, 80, 83,136, 24, 28,193, 80,187,182,188, 82, 22,128,
 80, 82, 83, 24, 46,154, 79, 38, 24,114, 74, 18, 79, 28,193,178, 26, 20, 77, 46,
149, 81, 32, 20, 44, 46, 26, 60,188, 20, 44, 80, 26, 81, 69,136, 86, 46,178, 26,
 44, 22, 20, 30, 26, 44, 80,128, 18,136, 26, 80, 44, 30,188, 77,108,193,180, 80,
179,187,192, 60, 20, 26, 44, 63, 80, 56,  2, 63, 81, 20, 78, 83, 79,164, 80,188,
 63, 26, 30, 81, 22, 63, 26, 44, 20, 80, 81,  4, 30, 63, 20, 26, 56, 22, 80,168,
 32, 79, 73, 20, 87, 44, 69,136, 22, 87, 26, 44, 46, 60, 20, 73, 30, 87, 81, 77,
149, 81, 14, 20, 26, 44, 32, 66, 73,136, 30, 77, 79, 20, 53, 30, 20, 73, 79, 77,
 87, 44,108,193, 80,192,180,185,187, 69,184,132, 79,138, 83,118, 63, 38, 79, 80,
 77, 78, 83, 18, 79, 80,188,184,187, 36, 64, 38, 36, 79, 78,136, 77, 66, 38, 79,
 80, 32,188, 36, 22,108, 80,193,185,192,187,180, 18, 80, 83, 77,184, 82, 78, 66,
 38, 80, 78, 83, 77, 82, 63, 38, 77, 80, 28, 30,136, 69,184, 46, 79, 83, 80,138,
 65, 30, 38, 77,128, 80, 78, 66, 77,108,193,192,180, 80,179,187, 59, 32,193, 49,
 56,187, 63, 26, 63, 56, 83,187, 81, 22, 22, 63, 80, 79, 82, 83, 30,  2, 63, 83,
 82, 56, 91, 78, 18, 56, 82, 83, 79,194,187, 79, 22,193,136, 86, 80, 82, 83, 67,
 20,193, 86, 77, 26, 30, 69,136, 86, 46,178, 26, 44, 59,193, 77,136,187,194, 26,
 73,174,180, 80,188,138, 26, 18, 83,194, 82, 26,192,193, 80, 73, 20, 30, 87,128,
174, 22, 18, 73, 82, 83, 79,136, 30,108,136, 22, 26, 30, 32, 44, 69,136, 22, 87,
 26, 44, 46, 67, 79, 26, 77, 73, 87, 30, 28, 73,136, 30, 82,193, 83, 22, 18, 80,
 14, 83, 77, 78, 82, 73,136,188, 77, 80, 79,174, 63, 38, 77, 80, 28, 30,136,108,
 80,193,187,180,255,255, 87, 38,194,180,193,136,255, 69,184, 46, 79, 83, 80,138,
 74, 69,125, 32, 72,143, 76, 75, 18, 75, 76,117,135,188, 72,154,121, 73, 22, 60,
 14, 67,168, 73,121, 32, 75, 14, 44, 67,135, 81, 32,117, 72, 73, 73, 32,145,117,
 81,180, 67, 82,108, 80,179,193,187,192,255, 69,136, 46, 79, 80, 30, 22, 67, 79,
 77, 20, 80, 26, 81, 68,136, 79, 75,193, 80, 26, 64,136, 79, 78, 83, 77, 80, 63,
136, 77, 79, 83, 78, 80,  4,136, 66,127,131,121, 80,145,141,149, 17, 77, 74,123,
 78, 19,150,127, 19,123,131, 77, 37,157,121,123,141,127, 74, 19,155, 78,121, 82,
145, 74,127,162, 78,131,141,127, 17, 79, 83, 10,136, 62, 77, 79, 80, 26, 67, 81,
 30, 78, 62, 77, 20, 66, 62, 82, 55, 30, 90, 76, 69,136, 30, 22, 46, 82, 77, 63,
 62, 69, 80,136, 77, 79, 68, 62, 30, 26,136, 20, 77, 82, 10,136, 61, 77, 79, 80,
 26, 12, 77,136, 30, 68, 75, 83, 67, 81, 61, 78, 30, 83, 77,108, 68, 77, 61, 79,
 81, 89, 68,136, 83, 77, 75, 80, 30,112, 61, 30, 83,136, 96, 80, 77,158, 83, 82,
 78, 81, 80, 26,112, 83, 80, 79, 82, 63,136, 67, 30, 63, 20, 26, 56, 22, 63,136,
 80, 79, 82, 81, 78,106, 82,136, 83, 91, 63, 81, 66, 63, 82, 56, 83,136, 30, 79,
 10,136, 77, 20, 80, 26, 30, 66, 82, 83, 77, 81,193, 78, 63,136, 30, 80, 77, 86,
 81, 64,136, 77, 30, 80, 86, 78, 69, 83,136,178, 80, 77, 82, 67, 30, 81, 78, 44,
 83, 46, 30,108,118,138,136,193, 38,132, 66,128, 82, 80, 83, 24, 38, 67,128, 24,
 38, 83, 82, 22, 20, 82,128, 80, 38,136, 83, 63,128, 38, 36,187,136, 22, 12, 80,
 82, 83,128, 24, 38, 79, 18,129,158, 32,124,180,138,142,134,160, 93, 32,138,120,
124, 78,149, 80, 32, 77, 26, 20, 93, 24, 26, 32, 78, 93, 44, 30,159,138, 80,120,
 30,142,134, 64,174, 78,120, 77, 93, 86,123,149, 26, 93,136, 83,188, 80, 66, 83,
194, 82, 26,192,193, 10,136, 77, 20, 80, 26, 30, 24,136, 82, 83, 77, 20, 30, 12,
136, 77, 30, 80, 83, 82,116, 72, 80,187,193,179,192,125, 64, 82,138,130,110, 77,
 86, 63,130,138, 82,110, 80, 44, 66,130,194, 20, 86,138, 80,158, 32, 81, 20,138,
 26,130,160,130,188, 32, 93, 26,138,162,188, 32,130, 78,180,138, 93,158,123, 79,
 32, 26,129,180, 67,123,192, 32,194,178,255, 36,129,123,125, 79, 22, 20, 10,129,
 86,123, 79, 20, 91,116, 72,129,192,193,179,255, 64,129,123, 79, 86, 22,125, 26,
 63,174, 38, 44, 86,143,129, 64, 86, 38,123,129, 44,174,116,129, 72,185,179,192,
193, 67,123,194,178, 81,192, 32,158,123, 38,125,143, 34, 20,150, 80, 18, 32,194,
125, 81, 77,158, 79,143, 56, 22, 80, 30, 63,123,129, 79,125, 80, 20,116,129, 80,
192,193,179,255, 67,123, 32,192, 81,194,178,150, 80,123, 32, 44, 81, 79,162,193,
 78,123, 79, 63, 80, 67,123, 28,193,178, 26, 20, 77, 46,149, 81, 32, 20, 44, 46,
 26, 60,188, 20, 44, 80, 26, 81, 69,136, 86, 46,178, 26, 44, 22, 20, 30, 26, 44,
 80,128, 18,136, 26, 80, 44, 30,188,192, 16,129,176,184, 80, 81,190, 74,184,129,
 93,125, 44, 46,155, 78,125,129,123, 81, 82,148,129, 78,184, 77, 81, 26,  4,184,
129,176, 44, 46, 93, 81,176,190,184, 46,125, 44, 32, 81,194,192, 81,255,255,255,
 28,125,129,143,184,194, 40, 22,129,125, 40,143,194, 77,  4,125, 40,129,143,184,
 77, 18,129, 77, 80, 26, 20, 44, 60,184,125, 40,194, 86,129,194,136,195,255,255,
255,255,255,168, 44, 72,129,190,125, 46, 18, 72,190,195, 46,191,189,108, 72, 80,
195,129,255,255, 53, 26, 72, 20, 44,192, 32, 63, 44, 46,178, 93, 72,192,178, 18,
184,255,255,255,255,255,116,184,179,255,255,255,255, 81,184,171,255,255,255,255,
 66,184,172,255,255,255,255, 22,123, 72, 26,190, 20,184, 65,123, 72,125, 32,184,
 20, 81, 81,194,192,255,255,255,255, 74, 44, 32, 30,192, 14, 22,108,129,192,193,
255,255,255, 18, 80, 32, 22, 14,129,123, 69,125, 44, 22, 46, 80, 74, 28, 74,123,
 77, 44,174,193, 68,123, 75,193, 86,138, 80,136, 44, 67, 20,193, 86, 77, 26, 30,
 69,136, 86, 46,178, 26, 44, 28,136,174, 80,193, 77, 26, 66,174, 77, 86,193, 80,
 93, 22,136, 83, 26, 80, 30,128,129, 75,192,193,116, 86, 44, 46, 69, 46, 86, 32,
 26, 44,192,149,116, 32, 26,124, 86, 77, 16,116, 26, 30, 86,120, 20, 28,174, 78,
193, 32, 46, 20, 26, 77, 30, 78, 86,120, 32, 86,108, 72,129,192,193,179,255, 75,
129, 32,123, 87, 72,125,154, 84, 32, 87, 93, 79, 26, 18,123,129, 87, 44, 20, 26,
 82, 32,158, 26, 72, 20,143, 67,123,192, 32,194,178,255,178, 66,184,172,255,255,
255,255, 82,184, 32,171,255,255,255, 65,123, 72,125, 32,184, 20, 75,184, 72,190,
194, 32, 93,108, 72,184,179,171,255,255, 67, 72,184, 20, 26, 30, 46, 32, 67,184,
 40,192, 77,174,182, 64, 78, 38, 86, 44,129,174, 69, 86,125,  0,129, 24,184, 63,
 86, 44, 77, 38,178,129,149, 81,193,123, 40,125, 78, 22, 24,125, 26,  0, 86,129,
125, 18, 26, 20, 44, 32, 46, 86, 64,130, 44, 86, 80,110, 46, 75,116,130, 44, 86,
138, 46,149, 32, 20, 44,116, 93, 80, 66,130, 77, 86, 93, 80,188, 63,116,130, 77,
 44, 80, 46, 66,123, 22,193,136, 86, 80, 82, 83, 67, 20,193, 86, 77, 26, 30, 69,
136, 86, 46,178, 26, 44, 59,193, 77,136,187,194, 26, 73,174,180, 80,188,138, 26,
 18, 83,194, 82, 26,192,193,129, 69, 46, 86, 32, 26, 44,192,149,124, 32, 44,192,
 26, 80, 73, 32, 46, 44,193,192,116, 28, 77, 26,174, 20, 32, 46, 64,174,120, 86,
 78,192, 93, 59,116, 26,192, 32,120, 44,194, 63, 44, 46,178, 93, 72,192, 67, 72,
 26, 44, 20,192, 32, 69, 26,178, 72,192, 32, 44, 64, 44, 46, 72,178,192,125, 22,
 72, 93,190,193,188,192, 68, 72, 26, 20,178, 32,191,125, 73,116,138, 80, 20, 44,
 86, 18,130,194, 20, 86,138, 80,168, 26, 44, 20, 80,116,188, 64,130, 44, 86, 80,
110, 46, 22,130, 81, 78, 77, 83, 80,154, 77,116, 86, 44, 20,188, 80, 73,123,129,
 87, 79, 20,125, 67,123, 32, 81,194,192,255, 18,123,129, 79,125, 87, 44, 59, 32,
125, 26, 20,123, 44, 63,123,125, 73, 79, 87, 77,108,125,123, 30, 22,143, 46, 86,
 18,123, 72,129,125, 79, 93,108,129,192,193,179, 72,180,154, 84, 93, 87,125,129,
 44, 22,123,129, 20,125, 72, 84, 73,129, 79, 46, 72,123, 20,168, 87, 72, 84, 44,
 20, 26, 64,123, 67, 20,193, 86, 77, 26, 30, 69,136, 86, 46,178, 26, 44, 28,136,
174, 77, 80,178, 82, 22,136, 30, 80, 86, 20,128, 66,174, 77, 86,193, 80, 93, 65,
136, 77, 20, 80, 26, 30,129, 69, 46, 86, 32, 26, 44,192,149,116, 80, 44, 78, 77,
 32,150, 78, 77, 32,138,114, 80, 28,174, 77,120, 78, 46, 86, 18,174, 78,120, 77,
 93, 86, 26, 77, 78,120, 30, 86,194, 86, 18, 72,123,129, 26, 79, 44, 78, 72,129,
 79,158, 26, 30, 67,123,192, 32,194,178,255, 69, 32, 26,123,129,125, 44, 22,123,
 87, 32,129,125, 20, 65, 32,129, 20, 72, 84, 87,125, 18, 82,138,130,110, 77, 86,
 71,116,130, 44, 77, 86, 78, 66,130, 77, 86, 93, 80,188, 63,116,130, 77, 44, 80,
 46, 78,116,158,130, 44, 80, 46, 65,130, 32, 44,116, 46,138, 80, 67,123, 32, 81,
194,192,255, 68,123, 79, 77, 87,129, 20, 63,123,125, 73, 79, 87, 77,108,125, 22,
143,123, 44, 87, 65, 77,123, 73, 79, 30, 87, 78,123, 87,125,143,129, 79, 93, 67,
123,192, 32,194,178,255, 69,125, 86, 26, 22, 14, 32, 65,129, 86,123, 79, 20, 91,
 68,129, 86, 79,123, 91,125, 18,129,123, 79, 86, 22,125, 28,174,123,125, 86, 79,
129, 63,123, 67, 20,193, 86, 77, 26, 30, 69,136, 86, 46,178, 26, 44, 22,136, 80,
 77, 83, 30, 82, 28,136,174, 77, 80,180, 26, 66,174, 77, 86,193, 80, 93, 65,136,
 77, 20, 80, 26, 30, 44, 67,123,194,192, 81,178, 32, 18,129,194, 86, 38, 36,123,
 84,178, 36, 86,193, 38,194, 26,174,125,129, 26, 28, 38, 22, 86,129,123, 38, 28,
125, 66,129,123, 38, 80,174, 86,129, 69, 46, 86, 32, 26, 44,192,149, 80, 44, 78,
138, 32, 77,150,114, 77, 78,138, 80,116, 28,120,116, 77,174, 26,193, 64,174,120,
 86, 78,192, 93, 77,116,120,174, 77,134, 30, 46, 84,194,178, 38, 86,192,255, 67,
123,192,194,178, 81,255, 18, 38,129, 86, 77, 93,143, 69,129,184,125,123, 86, 40,
 68,129, 40, 38,123,125, 86, 26,174,129,125, 38, 77, 86, 86, 18,123, 72,129, 79,
 44, 32, 77, 72, 79,129, 20, 26, 30,154,123, 84, 79, 93, 87, 85, 67,123,192, 32,
194,178,255, 69, 32, 26,123,129,125, 44,149,123, 72, 44,125,129, 79,125, 18,130,
138, 82,110, 80, 44, 64,130, 44, 86, 80,110, 46, 66,130, 77, 86, 93, 80,188,154,
 77,110,138, 44,130, 26, 77,116,130, 44, 78, 46, 80, 28,116, 32, 81, 77, 80, 26,
129, 67, 74,162, 26, 46,138, 44, 20, 22,116, 73, 75, 76, 46, 81, 30,148,114, 26,
 22, 20, 44, 72,155, 32, 26,124, 20, 44, 22,154,120,114, 32, 26, 44,134, 69, 72,
 76, 46,116, 26, 44,120,162,133,135, 74, 30,129,192,163, 74,129,135,187,133, 30,
 22,123,121,117,103, 22, 32,148,170,178,193,135,192, 74,149,105,115, 30, 74, 44,
 22,150,170,129, 44, 74, 30,192, 44, 81,187, 32,194,192,255,255, 74,188, 32,116,
120,138,182, 22, 74,192,184, 80,116,174,162,182,142, 74,134,138,124,149, 32,116,
 74, 80,134, 28, 69, 74, 83, 32,184,116, 82, 80, 74, 77, 79, 32, 44, 30, 78,108,
 87, 22, 14, 44,192, 46, 60, 77, 79, 44, 78,187, 32, 69, 79,125, 46, 32,143, 81,
168, 79, 87, 73, 81, 30, 66, 28, 66, 87, 79, 73, 22, 44,174,116,167,181,166,255,
255,255,162,192,153,195, 88, 67,160, 74, 74,166,124,180,167,255, 18,195,120, 74,
 95, 88, 67,108,120,116,192, 74,171,195, 66, 67,188, 95, 22,160, 88,124, 18, 32,
 22, 80, 30, 14,129,108,129,193,192,187,255,255, 81,187,194,192,255,255,255, 74,
 32,188,187,194,174,180, 28, 74, 77,187,193,174, 44, 69, 74, 22, 79, 14, 46, 83,
 18, 30,162, 18, 22,134, 80, 38,124, 67,116, 74,187,184,120, 80,108,120, 18, 80,
 67,194, 83,106,120, 18, 67, 83, 82, 38,116, 67,187, 24,185,184, 83, 30, 67,120,
 80,116, 74, 34, 22,162, 30, 28, 67, 38, 74,180, 63, 28, 38,120, 74,174, 80, 67,
 16,116,182, 74,184,120,168, 16,114, 30, 38, 46, 74,108,120, 30, 74,194, 46, 14,
134, 67,120, 30,194, 74, 46, 46,162, 30, 22,144, 67, 80,182, 67,116,182, 80,184,
120,192,134, 30,120, 74,194,144, 22,108, 30, 67,120, 74,194, 22, 66, 67, 30, 22,
 34, 74, 38,155,134, 30, 80, 22,124,187, 44, 64, 38, 36,120, 28,194,193,134, 32,
120, 67, 83, 38, 82,149, 32, 28,124,134, 38, 67, 67,116, 74, 80,184,120,193, 63,
120, 28, 38, 36,194, 80,126, 38, 67, 83,182,174, 36, 74,116, 26, 44, 20, 73, 46,
 22,162, 67, 26, 44,124, 46,142,149, 30,134, 72,188, 73, 26,155, 32, 22, 30, 72,
120,124, 72, 30, 73,180, 81,120,188, 10, 72, 30, 26,116, 44, 22,134, 64, 82,129,
121, 79, 74, 77,106,129, 30, 83, 32, 82, 74, 66, 82,121,129, 77, 78, 79, 10,121,
 32, 67, 22,129, 74,108, 77,129, 30,121, 78, 83,116,121,129, 80,187,193,192, 69,
 46,162, 34,116, 53, 40, 74,134,108, 74,183,184,185, 34, 83, 83,116,120,184, 74,
 67, 60, 67,116,120,184,144, 80, 74,155, 34, 40, 80,134,124,116, 66, 74, 22,116,
144, 53, 14, 83,108, 22, 30, 14, 26, 76, 46, 18, 26, 30, 44, 46, 14,120, 66, 62,
 69, 26, 20, 81,120,150,114, 22, 32, 44, 26, 30,149, 32, 81,134, 26,124, 14,155,
 32, 26, 80,116,124, 76, 74, 90,138,180,192,194,193,255,162, 32, 67, 46, 76, 75,
 26, 16,138,116, 76,120, 60,124,155, 32, 76,138,116, 72,144,154, 32,114, 75, 76,
120, 46, 28, 72, 76, 46, 30, 44, 60, 14, 90,138,194,192,193,255,255, 63, 74,134,
 80,120, 12,116,162, 46,  2, 30,116,138, 38, 83,184, 74, 60, 53, 67, 82,108,183,
 83,184, 82, 46, 74, 67, 74,116, 80,120,184,187, 82, 90, 89,138,192,193,194,255,
108, 22, 44, 80, 14, 46, 83, 18, 61, 32, 26,120, 75, 22, 55, 32, 20, 46,116, 83,
120,116,116, 46, 75, 83, 96, 32, 62, 32,116,120, 20, 83, 26, 22,162, 10, 82, 83,
 46, 67, 28,154, 46,184, 74,138, 53, 82, 16,182,138,116, 46, 60,120, 63,116, 28,
120, 38, 80,134, 18, 46, 83, 82,184,120, 74,168, 10, 16, 46, 30, 38, 60,108,193,
 67, 88, 74, 30,120, 22,191, 69,191,116,120, 83, 80, 22,123, 32,116,120, 14, 22,
 30,121, 67,116, 60,187,120, 32, 11, 30, 44,116, 74,138,120, 72,116,194,190, 30,
192,187, 74,162, 26, 22, 75, 53, 73, 44,117, 72, 67,174, 73, 60, 75,113, 60,179,
 67, 72, 75, 76, 15, 72, 22, 67,179, 26, 46,155, 72, 81,187, 22, 44, 26,103, 72,
 73, 22, 30, 44, 26, 67,155, 22,188, 30, 81, 66, 44, 66,187,116, 74,188,180,174,
162, 30,193, 44,187,180, 22,148,114,187, 53,194, 44,116,154,114,187,188,180, 66,
 74,168,114, 44, 30, 66,187,188, 22, 67, 16,120,184,193, 80,187,123,120,183,193,
 80, 82, 14, 66,120,183,187, 74,184, 80,154,193,120, 74, 60, 53, 67, 69,183, 83,
184, 82, 74,120,168,193, 16, 30,187, 74, 38,187,123,193,180,188,194,195, 74, 72,
194,193, 46, 30,116,159,162,116, 53, 67, 80, 44,124, 31,112,130,116, 67, 74,136,
 68,193, 82,124,181,195,255, 67, 67,116,193, 88, 30, 22, 30, 69,184, 74, 83,183,
 82, 46, 66,192,193, 74,188,187, 24,162, 67, 18, 53, 22,188, 36,155, 80,184,192,
 22,185,187, 63, 74,184, 67, 53, 60, 24,123,183, 18,184, 22, 80,193, 66, 74,162,
 32, 67, 26, 44, 20, 73,154,114, 32, 26, 20, 67, 46, 73, 67, 81, 30, 60, 53,116,
155, 72, 76, 32, 53, 75,116, 69, 72, 76, 46,116, 26, 44, 28, 72, 30, 22, 60, 26,
 44, 22, 73, 30, 38, 67,120, 60, 53, 63,116, 28,120, 38, 80,134,108,120,183,187,
 74,184, 80, 67, 16,120, 80, 74,116,184, 69, 46, 83,184,116, 74,120,168, 16, 10,
 30, 38, 53, 60, 32, 69, 84, 86, 85,120, 94,114, 73, 38,116, 86, 48, 94, 20,108,
193, 85,187, 80, 24,192, 67,116,120,187,193, 80, 38, 22, 84, 86,116, 38, 26, 20,
 65,116, 84, 85,120, 86,134,116, 18,121, 74,117,103,123, 77, 67, 32,120,130,102,
128,124,162, 78, 32,194,121, 80, 67,116, 77, 78,112, 79, 30,130,163,101,129, 80,
121, 32, 67,149, 14, 67,121,129, 80, 77, 44, 69, 74, 83, 32,184,116, 82, 67, 74,
120,116,184, 80,134,154, 32, 80, 74,142, 60,120, 73, 38, 67, 32,184, 36, 60, 63,
 67,116,120, 36, 28, 74, 59, 82,116, 74, 38,120, 36, 80,108,135, 32,125, 87, 30,
 46, 69, 79,125, 46, 32,143, 81, 73, 87, 81,135,117, 30,125, 59, 81,115, 73,125,
117,135, 22, 87, 81, 46, 22, 79, 77, 16, 87, 81, 79, 78,125, 30, 63,120, 22, 60,
117,121,103,123, 99,162, 67, 80,133, 78, 60, 53,163, 67, 78, 74,129, 80, 77,149,
 53,105,115, 22,129,133,148,170, 53,133,129, 74, 30,155,170,115, 74, 32,133, 78,
116, 18, 32,121,123,117, 80,103,116, 32, 82, 78, 83, 77,112,164,121, 83,129, 78,
 77, 79,162, 78, 77, 67,121, 79, 74,149, 67, 74,121,129, 32, 77,163, 67, 32,101,
 78,121, 80, 74, 69, 72, 76, 46,116, 26, 44,112, 30, 76, 67,120,116, 73,162, 67,
 73,116, 53,187,142, 65, 72, 30, 26,116, 44, 22, 68, 72, 26, 67, 44, 20, 75,155,
116, 32,120, 53, 60,142, 80, 69, 79,125, 46, 32,143, 81, 84,135, 66,187,125, 73,
117,108,115,135, 87,117,125, 32, 64, 87, 79,115, 73, 22,117, 68, 87,115, 73, 22,
 81, 77, 66, 87, 79,117, 77, 30, 44, 60, 69, 32, 46, 74,116,120, 26,108, 74, 67,
 26, 22, 46,187, 66, 26, 22, 74, 20, 44, 62, 67, 67,187,116, 32,194,192, 65, 74,
 67, 26, 20, 44, 22,162, 67, 59, 53,116,142,120, 67, 66, 32, 74,120, 26,174,187,
108,187, 65,174, 68,116, 74, 64,120, 65, 68,187,174, 74, 84, 65,174,193, 60,120,
116,155, 66, 53, 60, 32,114,116, 65, 66, 32, 30, 22,120, 68,125,108, 80,148,116,
130, 87, 32,110, 26, 64, 22,194, 32,130, 87, 46, 63,116,187,193, 44, 87, 26,168,
 87,116, 32,130,110, 20,121, 73, 87, 32, 26,130, 30, 15,116, 87, 46, 14, 22, 30,
193,121,116, 67,195, 79, 60,189, 31, 80, 30, 14,187,189, 32, 64,116, 22,187, 80,
 79, 60, 68,116,191,190, 80,138, 88, 15,116,187, 79, 32,189, 74,123,116, 14, 30,
 88, 22,195,192, 29,193, 78,255,255,255,255,123, 32,193, 30,195,189, 79, 17,116,
 79,193, 30,194,195,162,189,193,195, 67, 77, 30, 15, 79, 30,189,195,176,193, 63,
 44, 77,184, 78,195,116,187, 68, 82,181,193,195,255,255, 31,116, 82, 88, 95, 53,
 67,123,195,193,188, 74,180,255, 72,193,194,116, 30, 46, 44,168, 74,116, 79,193,
 88, 95, 69,116, 83, 88,193,181, 80,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255, 18,194,116, 88, 80,
187,193,195,255, 30,195, 46, 60, 82,116,191, 10, 32, 67,159, 88, 82,195, 34,195,
 82,190, 32,191,116,168,188, 32,187,116, 79, 44,148,116, 78,195,130,187, 44,130,
116,121,187,193,192,255,255, 66, 72, 75,188,117, 14,135,162, 72,121, 74, 75, 32,
 73, 26, 32, 62, 30, 55, 44,117, 12, 30, 72,121, 22, 32,135,108,135, 32,117, 72,
 69, 75, 79, 64, 82,138,130,110, 77, 86, 63,130,138, 82,110, 80, 44, 66,130,194,
 20, 86,138, 80,158, 32, 81, 20,138, 26,130,160,130,188, 32, 93, 26,138,162,188,
 32,130, 78,180,138, 80, 66, 30, 87,130, 26, 32, 46,168,130, 66, 32,110, 79, 87,
 63,130, 87, 82, 79, 44, 73,116, 32, 87, 30, 26, 20,187, 10, 73, 87, 32, 26,130,
 30, 30,116, 73, 87,138, 30, 26,187, 67, 46, 67, 30, 22,195,130, 72,116,194, 82,
 83, 80, 77,168,188, 32, 44,116,130, 88, 30,195, 67, 82, 46, 60, 44,134,194, 30,
 32, 79, 46,116, 10, 67, 32,159,116, 80,195, 67,168, 32, 60, 30, 66, 46, 74, 16,
 81,130,187,192,194,255,157, 32, 44, 46, 69, 30, 81,162, 32, 30, 44,110,138, 60,
 66,188, 53, 60, 69, 22, 74, 26,130,138, 32, 74, 53,194, 67,116,116, 77, 46, 83,
 82, 67, 14,164,174, 80, 46, 67, 32,125, 18,174, 67, 32, 46,123,121,162,174, 78,
121, 32, 46, 67,163,101, 67,174,129, 22, 32,149,174, 14,129,121, 83, 79,130, 74,
 32, 75,121, 44, 22, 14,154,121, 74, 44, 73, 62,125,162, 74,121,125, 69, 83,187,
116, 73, 32, 97, 30, 83, 75,126, 72,125,192, 71, 32, 73,108,121,193,192,187,255,
255, 74,108,193,180,179, 72,192,187,162,130, 32, 20, 44, 26,138, 74, 32,130,187,
180,194,178,168,188, 32,116,130, 81, 72,154, 32,130,188, 44, 20, 26, 69, 72, 26,
 30, 22, 44, 73,187, 68,195, 82,193,255,255,255, 16, 46, 67, 30, 22,195,130, 74,
 30, 82, 32,159, 44,116, 28, 82, 32, 30, 67, 46, 88,116,159, 82,194, 32, 79, 22,
 66,116,188, 67,159, 80,195, 46,108,192,185,187,255,255,255,168, 34,116, 30, 38,
188,130,154,116, 34, 74, 38, 40,188, 63, 79, 38,116, 77,187, 80, 74,116,188,184,
182,130,180, 18,184,187, 80, 22, 14, 79, 44,108,192,185,193,187,255,255, 74,116,
 32,188,182,138,184, 81,187,194,130, 32,192,255,154,130, 74,184, 36, 38, 32, 18,
 67, 80, 79,187, 78, 77, 64, 79, 36,116, 60,130, 38, 68,116, 18,121, 22, 30, 44,
 14, 80, 67,122,102, 74,130, 22, 14,164,121,131,129, 44, 14, 74,162,121, 74, 46,
 78, 44,131,163,174, 32,101, 67, 74,129,116, 78, 77, 32,112, 30, 79, 79, 18, 26,
 20, 44, 32, 46, 86, 64,130, 44, 86, 80,110, 46, 75,116,130, 44, 86,138, 46,149,
 32, 20, 44,116, 93, 80, 66,130, 77, 86, 93, 80,188, 63,116,130, 77, 44, 80, 46,
 32,108, 80,192,193,255,255,255, 75,116, 86, 24, 73, 80, 40,149,  8, 73, 80,116,
 66, 94, 65,116, 73, 86,130, 85, 84, 63,116, 86,130, 38, 73,187, 66,116, 24, 38,
 80, 94, 73, 74,108, 72,179,192,193,255,255,154, 26,116, 73,130,138, 81, 69, 72,
 26, 30, 22, 44, 73, 89,187,193,138,178,192,255, 65,116, 72,138, 73, 30, 26, 18,
 72, 26, 30, 44, 20,110, 30,108, 80,192,185,193,255,255, 63,116, 38, 80,130, 36,
 46, 75,116, 24,138,184, 79, 80,154, 79,138, 83,110, 24,116, 67,116,184,130, 74,
187,182,168, 79, 24, 80,116,  6,138, 44,108, 80,192,185,193,255,255, 64, 79, 36,
116, 60,130, 38, 67,116, 74,130,184,138, 38, 63, 79,116,130, 77, 28, 38, 18, 74,
130, 67, 79, 32, 77, 66,116, 80, 38,188, 74, 60, 66,130,154,121, 72, 73, 22, 75,
 14,162, 75, 73, 72, 14,121, 30, 18, 72, 75,188,117, 14,135,116, 73,117, 72,121,
192,125,108,121,193,180,192,187,255,126, 72,117,125, 74, 73, 71,116, 18,121, 74,
117,103,123, 77, 67, 32,120,130,102,128,124,162, 78, 32,194,121, 80, 67,116, 77,
 78,112, 79, 30,130,163,101,129, 80,121, 32, 67,149, 14, 67,121,129, 80, 77, 77,
162, 78, 63, 20, 80, 30, 84, 63,116, 32, 30, 22, 80, 20, 65, 32,116,138, 30, 26,
 22, 69, 32, 56, 79, 80, 81, 22,108,193, 80,192,187,180,179, 68, 56,116, 79, 26,
 32,138,188, 16,195, 77, 78, 22, 14, 88,108, 79,195, 53, 30, 60,187, 59,195,187,
 82,181,194, 74,106,130, 30,194,195, 46, 67,148, 53, 78,195, 95, 46,130,  2, 30,
 44, 67,130, 88,195, 74, 73,116, 75, 14,110, 26, 67,162, 73,110, 30,180,193, 44,
154,116, 73, 20, 67, 75, 30,108,192, 72,180,193,187,179, 59,116,193,187,130,138,
 32, 69, 72, 26, 30, 22, 44, 73, 80, 73, 87,116, 32, 20, 30, 14, 18, 30, 87,130,
 26, 32, 46,168, 32, 87,138,116, 30, 44, 65, 73, 87, 32, 26,130, 30, 28, 32,116,
 46, 30, 44, 26, 67,116,130,187, 81, 32,194, 69, 77, 18, 56, 22, 26, 46, 30, 79,
 63,116, 32, 30, 22, 80, 20,108, 80,192,193,179,187,255, 65, 32,116,138, 30, 26,
 22,  2, 63, 32, 56, 81, 44, 78, 66,188,116, 63, 56, 84, 91, 74,154, 32, 76, 44,
 26, 30, 75,162, 76, 32, 53, 26, 67, 44,108,179,193,180, 72,187,192, 28, 72, 26,
 73, 81, 75, 44, 65,116, 72,138, 73, 30, 26, 18, 72, 67, 26, 73, 30, 75, 80,168,
 32, 87, 26, 73, 83, 30, 65, 73, 87, 32, 26,130, 30,108, 32, 22, 30, 26, 14, 44,
 67,116,130,187, 81, 32,194, 63,116, 77, 87,130, 73,158,149, 81, 26, 79, 77, 66,
 73, 79,168, 32, 44, 93, 83, 26,116, 18, 32, 26, 44, 93, 46, 80, 64,130, 44, 86,
 80,110, 46,154, 32, 77, 26, 44, 93, 83, 66,130, 77, 86, 93, 80,188, 63,116,130,
 77, 44, 80, 46, 22,108, 80,192,185,193,187,255, 67,116,184, 74,130,187, 38,162,
 10, 30, 38, 28, 46, 14, 18, 77, 67,  4, 80, 74, 79, 66, 80,116, 38,130, 82, 67,
 68,116,184, 79, 80,130, 83, 44, 64, 79, 36,116, 60,130, 38, 67,116, 74,130,184,
138, 38,108,192, 80,185,193,187,255, 63, 79,116,130, 77, 28, 38, 66,116, 80, 38,
188, 74, 60, 65, 74, 79, 77, 32,116, 80,

115, /* b2-d3 */
129,124,194,129, 46,109, 30, 67, 60, 22, 93, 95, 46,144,142, 44,187,115, 32, 95,
180,152,120, 67, 68,116, 32,192,138,120, 82,109, 67, 32, 46,120,190, 95, 66, 32,
 46, 67,138,120, 88,124,142, 44,192,187,194,193,255, 18, 30, 80, 74, 22, 14, 44,
 72, 44, 30, 74, 80, 83,109, 93, 80, 74, 67, 60,174, 32, 26,123, 80,121,103,117,
 60, 16,109,111, 74,139, 80, 44,187,129,194, 80,195,181,255,255, 68, 82,194, 83,
 32,120, 22, 66,194, 32, 74, 67,120, 95, 72, 67,194, 82,144,120,116, 86,194,120,
 32, 80,144,181, 93,120,144, 95, 82,116,134,192, 93,142,255,255,255,255,255,129,
 30,194,174,176, 44, 14, 79,194,174,142, 44, 67, 60,109, 30,120, 44,190,142,194,
139,190,195,194,120,176,174,115,180,120,195,176,190,189,193,139,194,187,192,255,
255,255, 93,142, 44,187,255,255,255,129,194, 80,192,255,255,255, 18,194,120, 30,
187,116,181, 79,194,181,190, 32,189,195, 72, 30,190,189,181,195, 14,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
 79, 67,128,194,187,180,179, 30,193, 22, 74,116, 69, 60,124,187,148,114, 53, 74,
 60, 69, 26,  2,116,120, 60,187, 81,124, 28,116, 60, 74, 53, 68, 69, 16,120, 81,
 74, 69,188, 53, 83,128,194,193,180,187,179, 30,124, 26,124,194,192,193,187,162,
 69, 80, 55, 97, 76, 81,  2,116, 81,120, 69, 62, 55,170,158, 82, 76, 69,187,188,
154, 81, 80, 69, 82, 46, 55,180, 80,194,115,133,143,137,166,128,124,116, 80, 60,
120, 74, 18,116, 67,120,187, 80, 44, 26, 32, 80,120, 60, 46,174,162, 95, 44, 67,
 53, 46, 88, 72, 74,116,120, 67, 60, 53, 82,128,194,193,187, 30,179,255, 26, 81,
116, 83,120,187, 80,  2, 80,116, 81, 61,120, 54,124, 26, 75,124,194,187,192,162,
 75, 80, 96, 81, 54, 83,  4, 81, 80,116, 61, 89, 68, 74,128,194,193,180,179,187,
 30, 72,120,116,187,180, 32, 30, 18, 71, 73, 72,120, 26, 67,124, 26,124, 75,180,
179,193,  4,120,116,187, 73, 71, 32, 28, 67,187,116, 73, 26, 60,120, 22,113,117,
123, 60,103,121,148,129, 53, 74,135,188,133,120,102, 32,188,180, 80,124,155,115,
129, 74,135,180,174,163,115,129,135,133,180,105,150, 53,115, 80,129, 67,135, 86,
180, 79,116,120, 32,124, 80,134, 72, 74,116,120, 67, 60, 53,  2, 60,120, 67,166,
 74, 32, 93, 32, 95, 46, 60,138, 67, 85, 32, 95,116, 53, 60, 67, 65, 30,120, 53,
 44, 95, 67, 74,128,180,179, 30,193,194,187, 72,120,116,187,180, 32, 30,124,180,
179,124, 26, 75,193, 16,120,180, 60,116,174,134,162,180, 73, 71, 67,187, 75, 22,
174, 72,116,180, 60, 71,124, 26,123,117,103, 77,180,113, 72, 67, 80, 79, 78, 77,
 32, 79,109,111,180, 80, 67,129, 93, 80, 46, 30, 78, 77, 79,124,180,193,194,187,
192,255,128, 79,187,194,193, 30,178,138,124,194,193,192,255,255,255,128, 79,194,
193, 30,178,255,130, 87, 67,180, 80, 63, 68, 79,125,180, 80, 46,123,129, 18,180,
143, 79,123,125, 74, 72,123, 74, 79, 78, 77, 83,144,124,194,187,192,255,255,255,
130, 87, 67,180, 80, 69, 63, 79,135,131, 80,180, 67,187,128,194, 79, 30,187,178,
255, 65,131,129, 77,180, 30, 79, 18, 77,180,187, 83, 80, 79, 60,162, 67, 74, 53,
 61,180, 62,148,114, 53,192,180, 74,187,124,194,124, 26,192,193,179,128,194,187,
193, 30,179,255,155,180, 74,144, 61,120,124,154,180, 74, 61, 59, 81, 62, 93, 46,
 79, 67, 80,180,188,138,183,124,194,124,187,185,255,255, 86,180, 38,194,192,255,
255, 72, 74, 30,120,116, 40,183,128,194,187, 30,185,255,255, 65,180,120, 74, 53,
 67, 83,180, 79,116,120, 32,124, 80,134, 72, 74,116,120, 67, 60, 53,162, 95, 46,
144, 44, 74, 32,  2, 32, 95, 67, 46, 60,120, 96,138,172,188,174,255,255, 22, 46,
 95, 60,144,187, 67,188,162,144, 32,138, 46,180, 30, 72, 32, 74,187, 83, 67, 95,
 18, 95, 32,120,180,172, 46,130, 32,180,187,255,255,255, 65, 95, 53, 67, 60,180,
 30,  2, 95, 46, 67, 32,116,120,144,124,194,187,255,255,255,255, 79,135,131, 80,
180, 67,187, 65,131,129, 77,180, 30, 79, 22,187, 83, 60,180,188, 77, 86,131, 77,
180,187, 79, 80, 72,131,135, 79, 74, 82,129,187,162,144, 32,166, 46, 22,142,128,
 95,166,152,159,144, 67,124,120,144, 95, 82,116,134, 18, 95, 46,116, 32,180, 80,
120,166, 67,159, 32, 60,152, 72, 74,116, 32, 82,166, 67,120, 22, 22, 60,123,103,
117, 74,148, 53, 74, 46, 30,188, 78,120,102, 32,130, 30,180, 80,155,115, 78, 74,
 32, 30,187,163,105,115, 67, 46,129, 78,150, 53,115, 60, 74, 79, 67,162, 67,128,
 30,194,180,179,187,193,124, 26,194,180,124,179,187, 16,116, 66, 26, 46, 22, 44,
120, 30, 22, 66,194, 74,116, 66, 26, 32,116, 74, 53, 46,102, 74,180, 30, 32,194,
 66, 74, 72,187, 73, 32,116, 67,180,128, 30,180,179,194,187,193,124, 26,180,124,
194, 75,179, 79, 71, 73, 67, 44, 46,180, 86,180, 73, 71, 67,187, 75, 93, 71, 46,
 73, 75,180, 67,180, 72, 95, 32, 74, 67, 53, 60, 51,124, 53, 67, 74, 88, 30, 79,
 95, 44, 67, 53, 46, 88,  2, 32, 74, 67,116, 30,124,128,124, 67, 44, 46, 74,116,
 16,124, 30, 67, 60, 88, 74, 32, 79, 85, 26, 20,180, 24,188, 86,180, 16, 40, 24,
  0, 48,128,194,193,180,187,255,255, 72, 85,138, 80, 24,124, 84, 18, 85, 26, 24,
 40, 86,180,161, 84,116,124, 38,138,180, 53, 16, 26, 22, 50, 46, 67, 30, 51, 67,
180, 74, 26, 60, 30,  2, 67, 74, 51, 30, 81, 54, 86, 67, 50, 74,180, 49, 51,106,
 67, 74, 26, 46, 51, 22, 79, 67,180, 74, 50, 26, 46, 46, 18,144,180, 74, 40, 30,
 83, 86,180,194, 38,192,255,255,124,194,180,187,124,185,192, 26, 74,144,174,180,
 30, 53, 16, 74,144, 40, 67, 53, 22, 79,194,180, 67, 74, 40, 60,  2, 67, 79,116,
120, 60,187, 81,124, 66, 60,120, 74,194, 32,187,162, 66,116, 26, 30, 46, 74,154,
 30, 66,116,172,180, 60,106,120, 30, 68, 60, 69, 74,159, 60, 66, 20, 30, 74,188,
120,148,129, 78, 80,135, 74, 30,120,102, 80, 32, 67,124,188,155,115, 78, 80, 74,
 60,135,150, 53, 74, 80, 32,105, 83,157, 60, 30, 80, 74, 79, 82,255,255,255,255,
255,255,255,180,100,187,181, 74,174,173,255,162, 32, 74, 67,116, 30,124, 79, 32,
 46,187, 60,120,152, 72, 74,120,187, 60, 67,116,148, 32, 30, 95,114, 67, 88, 93,
 32, 95, 67, 46, 60,120,187,162,124,144,116, 46, 30, 32,124, 30, 46, 95, 67, 60,
194, 86,166, 60,195,116,120,180,128, 67,194, 60,120, 83, 82, 66, 67,180, 60,188,
 83,124, 72, 60, 74, 67, 30, 88, 32, 60,162, 67, 74, 59,114, 53, 61, 10,120, 67,
 53,188,180, 30,148,114, 74, 53, 81, 67,180,150,114, 53, 74, 67, 81, 30,255,255,
255,255,255,255,255,255,255,255,255,255,255,255, 74, 72,120,187, 32,188, 30,180,
 18, 67, 72, 73, 71, 76,180,124,180, 26, 75,124,179,187,128, 30,180,193,179,187,
194,  8,116, 76, 72, 67, 30,120,159, 75, 60,120,138,116,188, 77,150,123,158, 83,
 81,180, 70, 79, 30,166,108, 82, 79, 81,136, 80,152,108, 49, 82, 30,136, 83,110,
 79, 83, 63, 30, 84,136,124,192,180, 26,194,187,179,  2, 83,110,180, 81,187, 80,
125,148, 78,188, 30, 32, 81,130,166,130, 81, 80, 83, 79, 70,124,180, 26,192,194,
187,179, 18, 49,110,165, 63, 84,180,152, 49, 82, 30,116, 80, 81,153,116, 30, 78,
188, 70, 83, 49, 51,125,123,180,143, 32, 63,  2,123,125, 63, 77, 32,143,100,125,
123, 30, 63, 50,143,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255, 79,124, 26,194,192,180,193,179,128, 30,180,193,194,178,
179, 79,129, 66, 20,143, 32,123, 58, 30, 72,123, 26,129,125, 26,123, 72,129, 26,
 32,143,  4,123, 77, 32, 81, 20, 26, 46, 63,123, 83,129, 80, 30, 78,  2, 80, 83,
 79,123,125, 38,166,194, 79, 63,123, 82, 80, 79, 49, 79,172,123, 63,194, 26, 49,
123,129,194, 22,143, 58,125,123, 30, 63, 49, 80, 63,152, 49,123, 70,129, 77, 65,
166, 65,123,143,129, 32, 46,  2,123,125,129, 65,143, 64,124, 26,180,194,192,179,
193, 51,129,123, 65, 66, 30,125,  4, 49,123, 65, 77, 32, 14,128, 30,127,129,125,
 56,184, 78, 38,131,129,125, 79,184,182,183,121,123,129, 38,125, 56, 22,141,123,
182,183,129,194,125,123,123,129, 56, 79,183, 24,164,129, 79,123, 63,125,143, 79,
121, 77,123, 32, 86, 44,129,113, 32, 72,194,123,125, 86, 18,123,129, 32,194, 72,
192, 72,129, 32,123,192, 72,158, 26,123,129, 32,174,125, 44,  2,123,129, 32, 72,
174, 26,194,123, 79, 46, 30,143,125, 91,113, 79, 30,143, 91,129,125,  2, 79,129,
125, 46,143, 26,119,129, 79, 46, 78, 80, 22, 79, 46,129,125, 32, 79,143, 63,129,
125, 26, 79, 44, 46,178,113, 30, 14, 56, 79, 46,125,119,194, 30,160, 79,143,123,
123, 56, 32, 79, 22, 20, 46,  2, 79, 30, 56, 63,123, 20, 66,184,179,255,255,255,
255,115,180, 56,194, 30,172, 63,193,119,194, 79, 30,143, 80, 78,188, 32, 26, 84,
158, 44,123,  2, 32, 79, 30,123, 56,189, 18,194, 30,123, 79, 20, 44, 79,190,194,
 30,192,143, 20,113, 79, 30, 20, 91,179,158,179,143,143,171,178,255,255,255, 16,
173,255,255,255,255,255, 67,178,123,173,171,255,255,188,123,193, 30,129, 63, 70,
 79, 79, 30,129,171,255,255,119,123, 79,129, 30, 80,193,124,194,  2, 79, 30,129,
125, 32, 91,109, 79,129, 46,143,125, 91, 93, 46,143, 44,255,255,255,111, 79,143,
 30,129, 91, 46,164, 80, 30, 79,129, 46,190, 79,129,143, 79, 46, 56, 44, 26,131,
 79,143, 56, 82, 91, 38,127, 56, 91, 38, 84,174, 78,141,182, 56, 38, 84,143,123,
121,129,123, 38, 32, 56,125,123,123, 56, 38, 79, 80,180, 26,123, 56, 18,  4, 80,
158,179,139,171,178,255,255,255,255,188,123,129, 30, 70, 56,176, 79, 79,129, 30,
171,255,255, 64, 79, 30, 56,125,143, 63,111,123,176,129, 56,193,158, 72, 56,176,
 30,193,123, 14,193,139,194,192,255,255,255,255, 93, 44,143,255,255,255,255, 18,
194, 79, 30, 56,143, 26,  2,194, 79, 30,190,143,189,115, 30, 79, 91,172, 56,195,
188, 32,192,190, 22, 14, 30,192, 93,255,255,255,255,255,255,129, 79, 30, 56,176,
 91, 20,111, 79, 44,129,194, 32, 26, 63,129, 30, 32,125, 44,194,  2, 79,194, 30,
 44, 91,129,139,190, 79,176, 44, 56,174,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255, 16,123, 62, 20, 56,118,132,
 83,138, 60,192,178, 81, 56,188,174, 61, 26,136, 56, 84, 82, 81,124,180, 26,193,
194,192,255, 52,136, 82, 63,192, 83, 26,128, 79, 30,180,193,128,194,129, 60,192,
180,194, 32,116,255,148, 78,192,193, 32,116,124,150, 49, 32,180, 26,116,138,  0,
 32,116, 26,192, 30, 56,  8,193, 26, 20, 32, 44,116,155, 78, 32,116,124, 80,120,
 32,124, 26,193,194,255,255,255,148, 78,129, 80,143, 91,194,128, 79,193,194,255,
255,255, 62,125, 56, 63, 20,183, 38, 61,194, 56,129,125, 24, 49, 60,182,192, 56,
183,184, 81,192,130, 81, 32,191,193,255,255,  8,189,190,125, 83,176,123,128,184,
 79,191,193, 30,255,106,125,184, 84, 79, 81, 91,150,123, 49, 83, 80,184, 81, 24,
129,123,125,193,194, 83,193, 62,125, 83, 20,191,190,194,130,190, 80,189,191,125,
 49, 60,192,194, 81, 32,123,255, 10, 30, 79, 91,172, 56,195,106,123,125, 79, 80,
 81, 83,124,192,190,189,191, 79, 56,125, 60,192, 81,194, 32,130,178,148, 78, 81,
116,130, 56, 80,  8, 81,193,116, 82, 80, 32, 22,116, 56, 26, 81, 83, 80,128, 79,
 30,193,194,255,255, 24,116, 32, 26, 30, 20,193, 72, 56,106,123,129, 77, 63, 91,
125,162, 63, 77, 57, 49, 84, 91,163, 63,193,129,123, 32, 91,149, 14,158, 49,129,
143, 77,148,170, 49, 57, 91, 84, 70,150,170, 49, 77,143, 46,123,123, 74, 32,130,
 81,194,178,187, 70,194,136,188, 32,187,118,  2,180,188, 20, 44, 32, 56,128, 79,
180,178,128, 30,193,  4, 56,180,188, 32,187, 26, 22,180,187, 22, 32, 56,188, 26,
 74, 32,194,192, 81,180,178, 70, 32,129,125, 18,194,143, 71,123, 56, 38,125,129,
143, 73,129, 56,123,125, 38,143,128, 79,178,194,193,185,179, 22,123, 56, 79, 80,
 20, 44, 44,128, 79,194,178,193,185,179, 70,194,129, 32, 26, 28, 38, 71,129,194,
 38, 56, 36, 26, 75,129, 56,194, 38, 26, 32, 79, 32, 20, 79, 38, 56,125, 26, 56,
129,123, 26,125, 79,129,128, 79, 30,194,193,187,179,124, 26,124,192,187,194,193,
 74,180, 32,187,194,192,255, 71,116, 32, 44,180,120,138, 70,116, 22,180, 32, 30,
 44,  2,116, 56, 30,120, 44, 32, 20, 74, 32,194,180, 81,192,185, 71,123, 56, 38,
129, 26,125, 70,129, 32, 26,125,143,194, 73,129, 56, 38, 26, 32,125,128, 79,178,
194,193,185,179, 26, 26, 56,123,129, 79,125, 86,180, 87, 21, 79, 80,129, 25, 45,
 72,123,129, 56, 44, 79,143,  2,123, 56, 20, 80, 30, 44,  4,123, 32, 56, 30, 22,
 44, 22,123, 81,129, 32,152, 22, 93,129,152,123, 32, 46, 79,123,  2,180, 80,174,
136, 78, 83, 16,136, 83,180, 84, 56, 63,128, 79,178,180, 30,128,194, 72, 56,180,
 32,187,188, 26,124,180,179, 26,193,194,187, 22, 56,180,128,174,194, 79,158, 65,
 56,152,172, 26, 79, 82,100,159,152,166,255,255,255, 72, 56, 79,152, 49, 82,129,
124, 26,166,152,159,255,255, 84, 79, 56,129,123,143,125,106,129,123, 79, 80,125,
 30,143, 22,193,128,194, 56, 79,180, 84,158,172,180, 79,130,194,  2, 79,194, 81,
 83,128, 56,150, 81,172, 49, 84,130, 78, 16,194, 83,193, 81, 56,180,  4, 81,194,
193, 80, 79, 56, 56,148, 63, 91, 70, 77,143,125,150, 63, 49, 77,125,143, 84,106,
123,129,125, 84, 77,143,163,123,129,143, 63,193, 91,149, 49, 70, 77, 84, 91,158,
255,255,255,255,255,255,255, 79,128,180,178, 30,194,179,193, 79,129,125,172,158,
 20,174, 16,125,180,129,123,172,143,150,125,123, 80, 81, 30, 77,155,129, 81, 80,
125,192,158,148,192,172,174, 78,129,123, 30, 18,129,124,192,187,124,194,193,185,
 63,120, 46, 36,142,144, 38,162, 74, 18, 46,187, 82, 67,155, 80, 74,180,124,138,
144,120, 83,120,194,124, 82,192,154, 74,180, 80,188, 24, 82,125,148, 82, 80,130,
 79,180, 18, 30, 67,187,116, 12,  6, 24, 79, 32, 24,  0, 22, 28, 80,124,194,192,
185,187,193,255,168,110,180,138, 74,130, 80, 63,180,184, 79,183, 46, 14,123, 30,
 82,180, 83,110,132, 80, 24,180, 38, 46,128, 80, 24, 79, 32,180,185,187,183,193,
 26,180, 83, 79,174,110,136, 72,128,187,180, 18, 38, 80, 12,180,118,132,136,138,
 79,143,124,194,187,185,193,255,255, 30, 67, 82, 83,  6, 24, 26, 10, 79,134,180,
138,128,187, 79, 32,128,  0,  4, 22, 28, 63,180, 24, 36,128, 22, 38,120,138, 79,
134, 67, 83,130,180, 30,123, 79, 74, 67,143,129, 72,123, 79, 24, 74, 80,143, 12,
129,123,125, 60,143, 80,  2,129,123, 60,125, 38, 79, 24,129, 42,123, 60, 36,125,
148,123,129, 79, 95, 24, 14, 18, 79,125, 80,129,143, 67,123,124,194,192, 26,255,
255,255, 24,125,143,180, 79,123, 26,128, 79, 30,178,193,194,185, 14,180, 67,129,
125, 30, 24, 22,180, 30, 67, 24, 79, 80, 16, 60,124,194,193,255,255,255,255,155,
 24,143, 38,129, 12,  6,168,143,123, 81, 67, 74, 53,148, 53, 24,143,129, 38,123,
149, 14,180,172, 53, 24, 18, 10,123,129, 81, 38, 67, 61,129,124,194,124,187,192,
193,185,162, 80, 18, 36, 46, 74, 67,154, 80, 18, 74, 22, 36, 46,148, 53,193,124,
185, 22, 24,155, 22,180, 74,124, 18, 60, 22,183, 80,116, 38, 22,184,123, 62,128,
118,132,184,183,138, 10, 79,128,136,132,187,138,124,194,180,187,193,255,255,110,
 80,138, 24, 18,128, 22, 24,182,128,183,184,118, 22, 60,184, 18,128,183, 22, 24,
183, 66,129, 24,190,184, 38, 79,150,129,123, 22, 80, 82,143,124,190,184, 80, 46,
123,125, 22,129, 79, 74, 88,125, 53,154, 22, 38,125,143, 79,190,106,190, 80,184,
123,125,191, 38, 60, 67,125, 22, 74,123,178,124,192,194,193,255,255,255, 22,129,
 22,123,125, 77, 67, 24,129,125, 67, 32, 77, 74, 10,129, 77, 30,180, 79, 78,128,
193, 30, 79,194,255,255, 80, 60, 81,185,192,194,180, 18, 61, 73, 81,129, 82,183,
125, 24,185,183,192,184,125,123,124,192,194,185,193,255,255, 22, 18,125,123,185,
 24,  6, 62,125,183, 83,129,184, 81, 79, 32,150,129,185,123, 79,143, 20,148,129,
123,193,185, 79, 20, 65,129,194,125,193,180,123,155,129,123, 79,185, 66, 64,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,123, 16, 80,187, 26,185, 18,
183, 18, 32,180,185,187,183,193, 86,192,180,193,178,136,194, 81, 32,185,187,194,
192,255,  4, 32,183,185, 22,187, 24, 65, 79,128,136,132,187,138, 24,124,194,193,
185,192,255,255, 18, 80,180,125,183, 74,129, 78, 79,182,174,123, 30,143, 77, 80,
 79,183,174, 30,123,168,174, 32, 80, 18, 16,  8,110, 80, 32,123,129,  0, 67,  0,
124,194,193,185,192,255,255, 18,125,129,180,123, 32, 74, 77, 79,180,183,174,143,
123, 86,180,192,193,194,178,255, 78, 79,180,174,123,182,143, 65,180, 79,143, 74,
 67,125,129,124,194,187,192,124,193,185, 16, 26,  4, 18, 34, 60,120,162, 32,180,
 67,174, 10,187,128,193,185,180,194,187,255, 18, 32,180,187,188,193, 22, 72, 32,
185, 24, 74,182, 67, 28, 18,125,180, 80, 22, 36,123,124,192,194,185,193,255,255,
 78, 79,174,123,129,182,180,155,123,143,129, 44,125,185,168, 36, 10, 44, 16,183,
182, 86,180,193,194,192,178,255,124,194, 79, 32,190,123, 79,129, 22,115, 79,143,
 88, 95,190,195, 16,129,123, 46,190, 80, 74, 72, 80, 79,123, 74,129, 36, 18,125,
 79,123, 18,192, 95,109, 79, 95,125, 80, 36, 46,192, 72, 79,129, 74,123,184, 83,
 16, 38, 80,184,191,129,193,115,180,190,195,125,176,184, 18, 79,123,129,125, 18,
194, 66, 24, 38,184,191,129,193,109,129, 79,125,194,123, 36,185, 18, 79,192,125,
123, 74,129, 63, 79,184, 36, 60,183, 74, 16,184, 80,191,129,193, 24, 67, 79,123,
184,125, 74,129, 66, 24,184,191,129,193, 80, 26, 79,192,193,184,183,182,193, 18,
192,129,194,190, 79,185,129,129,194,192, 80,185,255,139,185,194,192,255,255,255,
 16,190,191,194, 80,123,189,162,129,123,192, 67,143, 18, 72, 79,123,194,185,192,
190,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255, 63,180, 70,123, 24, 22, 79, 74, 18, 66,123, 79, 60,
125,143, 67,130, 24,143,129, 22, 18, 38, 65,129,125, 79, 24,143, 38, 49, 53,123,
125, 12, 79,129,  4, 79,123, 60,125,143, 80,123, 65, 79,128,136,132,187,138, 16,
180, 79,128, 38, 83, 46,  4,128, 80,180,187, 38, 46, 77,187,180,128, 80, 82,183,
 84,180, 38, 36,136,193,192, 22,180,187,128, 38, 80, 82, 79,128,180,178,185,193,
194,255,150,123,125,180, 77, 38, 65, 16,180,123, 77, 65,194, 93, 65,129,194,125,
193,180,123,148,129,180, 78, 77,123, 38,255,255,255,255,255,255,255,143, 70, 24,
 22,184, 18, 82,128, 65, 79,134,180,138,128,187, 18,180, 24, 36,128, 22, 38, 77,
180,187,128, 60, 79, 80, 84,180, 38, 36,134,193,194, 49, 79,128, 67, 60, 24, 12,
 46, 70, 40, 79, 67,123, 77, 30, 77, 78, 80, 79, 82, 40,125,130, 73, 80, 67, 68,
 69, 94, 84,180, 38,194,178,192,255, 49, 14, 77, 74,123, 78, 53, 65,129, 77, 30,
123,125,180, 12, 77, 79,123, 30, 82, 78,143, 70, 77, 60,123, 79, 82, 42,124,194,
192,185,193,255,255, 65, 30,180, 79,143,129,123, 84,180, 36,193,194,192,178,130,
 80, 73, 68, 87, 69,180,130, 73,117,129,125, 72,193, 24,184,115,129,123,125,143,
 71,193, 67,123, 24, 42, 22, 14, 36,139,123,129, 24,143, 38, 72,121,123,129,125,
 24, 38, 74, 26,129,123, 72, 71, 24,125, 67,115,125,123, 24, 81, 18,129,117,129,
123, 18,184, 81,185,121,129, 24, 74, 38,123, 18,139,123,129, 18,184, 81,185, 16,
123,129,125, 38, 24, 68, 79,123,  0, 24, 32,129, 22, 80, 72, 79, 81, 24,143,125,
185, 16,129, 73,125, 79, 24,143,115,129, 79, 73, 24, 81,123,121,123, 79, 73,125,
129, 38, 67, 79,125, 24,143, 18, 42,117, 79, 73, 81,123,129,125, 68, 67, 24, 14,
 36, 42, 22, 46,117,123, 79,125,129, 80, 67,115, 89,123, 67,129, 61, 54, 79, 32,
129,  0, 24, 67, 28, 16, 61,129,125, 18, 24, 75,139,129,125,123, 67, 38, 89, 69,
 67, 24, 14, 22, 36, 42, 38,117, 68,129,123, 83,125,183,115, 67,129, 68, 38, 76,
 18,139,129, 38, 67,125, 68, 83,121, 24, 76,129, 22, 14, 12, 79,  0, 32, 24, 67,
 22, 28, 87,117,129, 73, 80, 18, 38,123, 67, 38, 22, 86, 14, 24, 36,115, 73, 38,
 24, 86, 18,129, 79,  0, 22,129, 28, 24, 32,121,123, 73, 38, 80,125,129,135, 38,
 73,123, 86, 24, 18,180, 72,123,128,136, 80, 32, 79, 44, 46, 16, 30, 77,136, 80,
 32, 74, 79,152,136, 44, 80,138,132, 22, 77, 22, 79, 32, 74, 80,  4, 77, 79,152,
136, 32, 74, 93, 46, 77,138, 95, 32, 80, 74, 73,176, 73, 67, 75, 95,143, 79, 81,
129,174, 32,188, 30,155,129, 30,143,125, 44, 20,154,129, 73, 32, 20, 26, 30,148,
129, 73, 26, 30, 53, 44,124, 32,129,125, 44,174,143, 79,128, 87,166,129,152,143,
 44, 22,123, 32,129,125,143, 93, 26,123,174,129, 44,143, 80,  4,123,129,125,143,
181, 20, 28,123,152, 86, 32,188,129, 16,129,123, 44,143, 78, 77, 67, 75, 26,181,
188,255,255,255, 73,123, 95, 88, 20, 46,166, 22, 60, 74,123,129,143, 44,  4, 60,
 74,123,129,143, 44, 16, 60,152, 44,143, 20, 74, 79,188, 81,129,174,172, 32, 77,
 16,123, 44, 56,143, 63, 49, 70,129, 79,143, 84, 91,152, 71, 79,129,123,143, 44,
 22,  2,123, 56, 63, 80, 49,178, 22,123, 79,129, 56, 63,176, 75, 26,181,188,255,
255,255, 60, 75, 26,181,188,255,255,255, 73, 67,123, 88, 95, 62,176, 79, 81, 32,
129,174,188, 30,155,123, 32, 74, 59, 44,143, 93,129, 95, 46, 81, 74, 22,148, 74,
 53, 67, 44,143, 88, 79,129, 80,194,115,133,143,137,166,128,124,116, 80, 60,120,
 74, 18,116, 67,120,187, 80, 44, 26, 32, 80,120, 60, 46,174,162, 95, 44, 67, 53,
 46, 88, 72, 74,116,120, 67, 60, 53,143, 26, 44, 80,134,187, 46,138, 80, 32,128,
130,166,134, 46,148,187, 80, 46,130, 30,188, 16, 44, 80, 46,134, 30,130, 18, 80,
 44, 46, 67,134, 30, 77,128, 30, 78, 79, 80, 44, 80, 22, 32,123,143, 44, 20, 26,
  4,125,123, 32,143, 87,188, 16, 32,143,125,188,123, 20,  2,123,125,143, 81, 87,
 32,148, 32,143,166,123, 87,125, 26,123,125, 32, 26,143, 44, 44,  2,129,125,123,
143, 38,152, 26,129,143,123,125, 80, 38, 18,129, 80,123,125,143, 67, 22,129, 80,
125,123,143, 38, 16,129,125,143,123, 38, 80,148,129,123,143, 80,125, 38,125,162,
166,130, 80, 44,116,110,  2,187,116, 46, 80,152, 44, 18,187,166, 80,110,152, 44,
148,116,110,187, 80,166, 44, 26,116, 80,110, 44, 30,187, 80,194,110,187,116,166,
152, 46, 18,129, 40,125, 80,123,143, 26,129, 38,123,125,143, 40, 80,152, 95, 38,
143,166,125,  2,129,125, 38,143,123, 95, 22,129,125,143, 40,123, 38, 72,123, 74,
 79, 67,129,125, 18,129, 72, 74,120, 95, 88,116, 32, 79,116, 67,120,187, 80, 44,
 93, 95, 46,116, 32, 22,144, 30, 30, 60, 95,116, 74, 67,162, 95, 74, 88, 46, 32,
124,  2, 30, 60,120, 22, 32, 67, 77, 30, 79,123, 56, 30,181,152,  2,123, 32, 46,
 22, 20, 79, 12,123, 32, 56, 22, 44, 46, 72,123,129, 56, 44,143, 20, 79, 79,188,
129, 30,174, 32,158,129,123, 56, 32, 44, 78,123,128,136, 79, 80,188, 44,187, 30,
136, 79,152, 30, 80,110, 79,152, 80,136,110, 44,187,160,136,128,132,138, 74,110,
106,136, 77, 80, 30, 79,194, 24,136, 77, 79,128, 30, 80,143, 72, 77, 80, 74, 79,
 88, 95, 79, 80, 44, 46, 67,134, 30, 12,194,187, 60, 32,188, 74,150,128,138, 80,
 78, 77, 32,158,128, 80, 32, 95, 30,134,157,128, 80,134,138,130, 77, 79, 66,123,
125,143,192,129,174,128,129,188,125,143, 32, 26, 30,129,188, 60,143,125,166,160,
129, 80, 81, 72, 93, 78,158, 32,129, 86, 77, 78, 81, 26, 72,123,129,125, 86, 32,
125, 72, 74,130, 79, 80,187, 78, 79,187,166, 80,110,152, 44, 93, 32, 46,166,138,
 95, 80, 12,116, 32,187, 30,188, 60, 10, 30,116, 77, 79, 32, 80,  2,130, 79, 32,
187, 60, 30, 16, 60,150, 61, 62, 67, 74,172, 88,162,123,143, 32, 20, 44, 26, 32,
 32,123,125,143,181, 46, 66,123, 32,125,129, 67, 95,168,129,143, 32,123, 20, 26,
 22,129, 32, 61, 20, 46, 26, 67,162, 60, 26, 68,143,129, 20,168, 26, 68, 60, 74,
129,143, 22,129, 20,125, 26, 60,123, 61, 68,172,174, 26,181,188, 66,129,125,188,
143, 30, 32, 24,129, 26,125, 68,143,123, 74,130,125,129,123, 30, 81, 22, 61, 75,
 26,174,181,188,255,120,192,176, 32, 26,123,177, 24, 26, 67, 75,176,129, 60,106,
 32,192, 26, 20,176,123,110, 32, 26,192,176,177,123,125, 22, 74, 67, 79, 60,188,
116, 60, 67,188, 74,192, 79,116, 61,188,181,172,110,255,255, 66, 30,194,192, 60,
188, 46, 24, 60, 67, 79,116,188, 88,  8, 79,181, 60,116,188, 30, 88, 32, 87, 32,
 89, 95,125,129, 61,188,172,174, 26,181,255, 22, 60, 67, 74, 26,176, 20, 66, 67,
125, 87,129, 44, 46, 24, 26, 67,176, 60,125, 20,  8, 60,176, 26,175, 67, 74,188,
 62, 83,125,194,255,255,255,130, 32,195,181,255,255,255, 22,125, 60, 79, 83, 67,
195,168,125, 60, 74,123,167, 79, 24,125, 79, 60, 83, 67,195, 66,129,125, 79, 46,
 95, 67,  2, 30,100,123,174, 74,173,181,255, 72, 60,123, 24, 74, 18, 67,154,129,
123, 18,125, 38, 74,  8,129,123, 79, 18,125, 24, 18,129, 79,123,125, 24, 38, 10,
129,125, 79, 24,143, 38,129,100,187,181, 74,174,173,255,162, 32, 74, 67,116, 30,
124, 79, 32, 46,187, 60,120,152, 72, 74,120,187, 60, 67,116,148, 32, 30, 95,114,
 67, 88, 93, 32, 95, 67, 46, 60,120,123, 93, 79, 32, 95, 46,136, 80, 79, 80,136,
110, 44,132,138, 66,136,128,132,110,118,138, 18, 77,136,152, 79, 80, 74,  8, 79,
 30, 32, 22,136,128, 14, 79, 32,136, 80,118,132,143,100,187,181, 74,174,173,255,
 18,128,134,194, 77, 32, 22, 79,152, 80,188,187, 44,138, 72, 60, 74, 67, 79, 80,
 53,150, 80,128, 30, 32, 67,134,148,128, 78, 32,138,130,194, 77, 79, 79,129,172,
174,188, 30, 63,129, 79,188, 78, 80,123, 72,123, 56, 63, 80, 49,178,160, 56,123,
 80,129, 30, 44, 14,129, 32,125, 80, 44, 81,  8, 79,129,125, 81, 78, 80,125, 79,
187,116, 46, 80,152, 44, 72, 79, 74, 60,130,116,187,148, 78, 32,138,110,116,130,
 18, 77,130, 78, 32, 30, 79,162, 80, 78,116,110,138, 79, 64,130,116, 22, 32, 79,
138, 66,188, 59,195,194,181, 82,123, 74, 18,125,195, 83, 67,194,123,127, 83,125,
123, 79, 60,160, 16,129,125, 79, 46, 95, 67,119, 83,194,125, 79, 67, 77, 99,125,
 79,174,194,129, 77, 60,101,188,192,129,125,143, 22,119, 30,123,188, 46,129,125,
 99,129, 26,188,125, 46, 30,105,129, 32,123,143, 26,188, 59,173,181,179,174,123,
 74, 16,123, 32,125,129, 67, 95,192, 65,176,190,195, 44,193, 30,123,194, 79,176,
193, 82,125, 80,190,194, 80, 32,195,125, 73,194, 77,184, 74, 79,125, 59,191,190,
189,125, 83, 77, 69,125, 77,184, 46, 79,194, 67, 59,123,179,181,173,174, 66, 16,
129,125,188,143, 30, 32,101,125,176, 66, 60, 88,188, 67, 87,125,129,143, 22, 32,
 65,129, 32,125,188, 46, 60,109,123,125,143,129,174,192, 79, 59,174,173,179,181,
123,255, 18,123,125,143,192,129,174, 16,188,129,192, 72, 81, 86,  4,129, 77, 32,
125, 81,192,  2,129, 77, 32,188,125, 26, 65,129,123,125, 32, 77, 26, 77, 59,181,
179,173,174,123,255,119, 79,125,188,123, 26,129,109,192,188, 79, 56,143,174, 16,
 56,192,123,174, 30, 70,101,192, 79,123,125,188, 56,  2,125, 32,188,129, 22, 79,
 79,128,180, 72, 87,166,129,152,143, 44, 18,129,188,125,143, 32, 26, 79,129, 30,
 32,174,188,255,148,129,188, 87,125,143, 20,113,188,129,125, 30, 32, 22,  2,129,
125, 20,166,143,178,178,113,180,166,172, 14, 22, 32, 64,180, 30, 59,179, 32, 26,
 63,180, 30, 73,123, 67, 32,143,166,172,180,125, 32, 59, 79,177,171,255,255,255,
255,119,166,180,172, 14, 32, 22, 30,164,129,125,123,143, 22, 18,127,123,129, 73,
 94,180,172, 26, 67, 73, 68,178, 69, 80,141,129,123, 73,158,125, 67,145,123,129,
 80, 73,158, 69,121,129,123, 73, 80, 68,125,194,164,129, 32, 46, 26, 44,193, 64,
 87, 73, 67, 80, 68, 69,113,188, 87, 67, 80, 68, 73, 18, 87,188, 80, 73, 67, 59,
 79, 87, 73, 67, 80, 68, 69,123, 87, 69, 80, 67, 68, 73,193,164, 32,123, 26, 44,
125,143, 79, 87, 59, 26, 80, 32, 67, 16, 80, 67, 87, 73, 68, 69,  2, 87, 73, 67,
 68, 69, 80, 64, 87,158, 73, 67, 68, 80,113, 87,172, 73, 80, 67,158,179,164,193,
143,123, 44, 46, 30, 79,171, 80, 30,129,255,255, 26,178,180,171,173,255,255,148,
180, 87,158,178, 65,167,  4,178,180, 65, 87, 73, 67, 64, 87,158,180, 65, 67, 68,
124,194, 18,129, 32, 44,125, 86,143,115, 32,125,129,195,193, 72, 72, 32,192, 72,
 93, 44,190,  2,129,125, 44, 32, 86, 72,109,129,125, 32, 86, 26, 44,139,190,125,
129, 32,195,189,192, 18, 32,129,194, 72,125, 44,115,195, 72,193, 86,176,180,129,
194,190,125, 32,184,195, 72, 32,125, 86,129, 72,194,139,190,129,125,195,189, 72,
109,129,125, 72, 26, 32, 86, 26,127,129, 32,125,184,123, 72, 26, 18,123,  4,158,
178, 80,121,123, 32, 44,129, 77, 38,141,129,194,125,193, 72, 32,131,129, 32,125,
123, 72,184,123,123, 72, 38, 20, 44,129,193,139,192,194,255,255,255,255, 18,194,
129,192,190,195, 26, 72, 32,192,194,195,125, 26,150,123,192,190,129, 72,194, 16,
192,125,191,190, 78, 72,115, 32,123, 72,195, 20,143,179, 72,193,129, 44, 46,123,
 30,188, 30,123, 46,193, 72, 44,139,178,171,255,255,255,255,150, 86, 93,172,180,
193, 72, 18,123,125,129,143, 46, 72,115,180,171,172,178,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  4,
123, 72,180,128,194,187, 32,188,124,180, 26,179,194,187,193,128,178,128, 30,180,
187,194,158,128, 30, 83, 26, 81,136, 66, 72, 83, 82, 32,136, 86,110, 77, 65,136,
 26, 72,128,193, 10, 32,123, 72,195, 20,143, 20,125,129,123, 32,189, 26,148,123,
 32, 26,194,195, 72,128, 87, 73, 68, 69, 67, 80, 66,123,129,194, 77,190,125, 72,
129,194,125, 32, 26, 44, 32,160,125, 72, 26, 40, 44, 38,158, 81,125,194,193,143,
129,120,129,125,192,183, 72, 81, 72,194,129,123, 86, 26, 40,124,192,185,194,193,
 26,255,159,125,123, 81,193, 72,185, 77, 16,123,194,192,178,180, 32,160,123,125,
 83, 81, 79, 80,150, 49,123, 46, 32, 44, 81, 86,123,143, 56,193, 79,180,159, 82,
123, 81,125, 78, 79, 63,125,129, 79,194,123, 32,194, 16, 77, 32, 83, 26, 20,125,
150,123, 46,129, 30, 44,143, 20,125, 46,193, 77,143,189,120,125,129, 32, 44, 26,
 72,148, 32, 26, 46, 20, 78, 44, 72, 32, 44,123, 26, 20,143, 26,128,180,178,193,
179,185,194, 12,129,193,194,143, 72,123, 10,123,180, 72, 32, 77,194, 16,123, 18,
180,185,194, 81,150,129,123,125, 18, 72, 32,160,129, 18,123, 72, 83,  2,  2,123,
128,128,178, 30,180,187,194,124,180, 26,179,187,193,194,158, 83, 72,128, 81, 26,
136, 66, 72, 83, 82, 86,136,174,100, 32, 26,136, 44,193, 77, 72, 72,180,187,194,
128,188,193,128, 87, 73, 67, 68, 69, 80, 34,195, 72,189, 32, 83, 86, 26,123, 32,
125,129,172, 72, 66,123,190,194, 77,129,192, 10, 32,123, 72,195, 20,143, 72, 32,
195,194,125, 26,189,129,128,180, 30,187,194,193,179,159, 80,138,120,124,142,134,
124, 26,194,192,193,187,124, 66, 72, 20,114, 46,192, 32,160,116, 72, 26,187,120,
193, 14, 72,116,187,180, 26,120,194,158, 32, 46,143,191, 20, 26,150, 30, 46,123,
 44, 81,193, 26,123, 26,125, 44, 46,143, 18,129, 32,143,123, 26,125,148, 32, 44,
 26, 20,125,188, 72, 44,123, 20, 32, 26,143, 72,154,123, 22, 44, 30, 20, 65,162,
123, 32, 44,143, 26,125,124,192,194, 75, 26,179,193,148,129,123,180, 32, 71, 22,
128, 30,194,178,193, 79,179, 34,180,129,123, 32,193,192, 32,128,178,180,185,194,
193,255, 18,129,180,194, 72,174,123,150,123, 81,125,129, 80, 72,160, 72,125, 26,
129, 40, 80,  8,129,123,180, 72,193,194, 14,125,123,180,143, 72,194, 16,123,128,
128,178,180, 30,193,194,124, 26,180,194,192,193,255,155, 78,114, 80, 32, 46, 20,
 66, 65, 32, 77,136, 83, 26,149, 65, 77, 78, 83, 82, 20, 10, 65,136,180,187, 72,
 20, 77, 60,192,194, 81,178, 32,180,128, 30, 79,194,193,255,255,  8,129,125,123,
 81, 26, 20, 61,125, 32, 56, 82,123, 26, 62,125, 32, 20, 46, 81, 26,148, 81,123,
 78,129,143, 83,125,168,116, 72,130,110, 81,138, 60, 81,180,194,130,178, 32,128,
180,178,193, 30,194,255,148, 78, 81,130,110, 26, 65,120,116, 77,192, 32, 44, 81,
124,192,194, 26,193,255,255, 26, 60,194, 81,185, 32,180,178,128,178,180,185,193,
194,255, 10,123,180, 72, 32, 77,194,150,129,123, 83, 80, 65, 18, 62,125, 83, 81,
183, 77, 20,148,123, 81,129, 78,143, 86, 78, 60, 81,192, 32,178,180,194,128, 30,
194,193, 79,255,255, 22,123, 77,125, 26,129, 46, 61, 82, 81,125, 26,143,129, 62,
125, 77, 81, 20, 83, 82,124,192, 26,193,194,255,255, 72,162,123,143,125, 26, 20,
 44, 60, 74,192,123,180,178,194,154,123, 20, 22, 26, 44,125,124, 26,192,193,194,
 75,255, 62, 76,125, 32, 20,180, 26,150,129, 73, 76, 71,123, 75, 72, 20,128,178,
180,185,194,193,179, 26,129,183,193, 72, 26,143, 22,129, 72,143, 38,123,125,162,
123, 78, 83,125,143,129,  2,194,143, 72, 86,123,125,106,129,123, 32, 38,192, 44,
125, 22,116, 86, 72,130, 32,180,128,178,180,179, 30,193,194, 28,116,130, 86,194,
 93, 26,162, 78, 77, 81, 32, 20, 82,124,192,193, 26,187,194,179,  4,116,130, 86,
 93, 72, 26, 86,124,192,194, 26,193,179,255, 22,125,129, 72,123, 79,143,128, 79,
 30,193,178,194,179, 26,123, 72, 26, 32,193,129,  4,125, 26, 32,129, 87, 14,  2,
125,129, 87, 20, 72,123, 72,162, 79, 20,143,125, 44, 26,150,129, 79, 74,123,125,
 44, 16,129,125, 20, 79, 32, 74,155,129, 71, 20, 74, 79,123,148,129, 71,158, 26,
 79,123,255,255,255,255,255,255,255,180,128, 87,166,129,152,143, 44, 22,123, 32,
129,125,143, 93, 26,123,174,129, 44,143, 80,  4,123,129,125,143,181, 20, 28,123,
152, 86, 32,188,129, 16,129,123, 44,143, 78, 77,158, 65,193,152,172,129, 44, 46,
124, 26,159,152,166,255,255,100,159,152,166,255,255,255,128, 73,123,129, 63, 80,
 87,120,129, 72, 32,125, 26, 20,102,129, 72,123, 32,125, 26, 78,128, 30,127, 73,
 86, 38,129, 94, 24, 16,129,123, 77,125, 71, 24,121, 79,129, 24, 71, 22, 12,131,
129, 79,184,183, 71, 92, 26,129, 79,125, 24,123, 71, 66,123, 24, 79,174, 77, 12,
 79, 16, 80,174,193,123, 20, 44,122,192,194,178,193,179,255, 18,123,174, 32,125,
 72,129,155, 32,174, 80,129, 26, 72,  2,129,174,123,125,194, 46,150,123,125,129,
 30, 32, 72,194,113, 79,125,129,143, 80, 20,123, 79,143,129, 82,125,190,  2, 79,
 46,129,178, 77, 44, 26,129, 77, 46,125, 79, 71,143, 79,193, 77, 80,143, 46, 16,
 77, 85,170, 71, 92, 46,178,168,184, 79,157, 71, 30, 77,113, 22, 79,123,125, 14,
 20, 18,194, 30,184, 79,166, 20,123, 79, 14, 32, 20,180,190,119,194, 30,143,160,
 79, 22,143, 79, 30,160,194,190, 22,193,123, 79,190,179,143,189, 77,  2, 77, 32,
 79, 71,191,194, 69, 79, 30,123,191,158, 26, 18,194, 79,123, 77,129,191, 26,194,
 32,123,129, 79, 71,188, 32, 26, 85,158, 44,123,179,143,143,171,178,255,255,255,
 16,173,255,255,255,255,255,168, 79, 85, 92, 57, 50, 71,188,123,193, 30,129, 64,
 71, 67,123,178,173,171,255,255,  2,123,178,193, 30, 79, 46,124,194, 93, 46,143,
 44,255,255,255,115,188, 79, 32, 77, 80,195,139, 79,190, 46,143,125, 77,109, 79,
 92, 32, 77, 46, 22, 72, 79, 30, 14, 32, 77, 81,111, 79, 92, 20, 77, 32, 46, 26,
 26,123, 18, 80,  4,158,151,131, 79, 38,129, 80,182, 44,121, 32,129,123, 77, 79,
125,168, 81,185, 82, 83, 50,158, 86,192,178, 85,174,158,123,123,123, 79, 80,180,
 38,129,179,139,171,178,255,255,255,255,188,123,193,129, 64, 30, 71, 79, 79,129,
 30,171,255,255,111,123,129,193,158, 71, 30, 64, 30, 79, 71,129, 22,193, 93, 57,
123,143, 30, 79,129,193,139,194,192,255,255,255,255, 93, 44,143,255,255,255,255,
 18,194,190, 79, 30,143,129,188,190, 32,192,189, 22, 14, 72, 79,194,190, 30,143,
129, 69,190, 79,192,189,125, 30,192, 93,255,255,255,255,255,255,129, 79, 30,190,
 92, 26, 20, 18, 79,194, 44,190, 26, 30, 72, 79, 30, 92,190,194, 44,  2, 79,194,
 44,190, 92, 30,115,180,195, 30, 20, 92,190,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255, 72, 57,149, 50, 78, 85,
 71, 56,123,155,170, 14,123,129, 44,125,163,123, 56, 78, 85, 50, 92,106, 73, 63,
158, 67, 68, 69,150,170, 50, 46, 14, 20, 85,255,255,255,255,255,255,255,123, 74,
 32,130,178, 81,194,187, 22,187, 77,180, 22,136, 81, 79,136, 79, 20, 26, 77, 32,
  4,187, 77, 81,180, 71,188, 28, 57,187, 32,180, 81, 20, 71, 77,187, 32, 80, 30,
 20,129,124,124, 26,192,194,193,187,128, 79, 30,194,193,187,179, 74, 32,187,194,
180,192,255,106, 71, 92, 80, 77, 79, 85, 71,120,116, 32, 80,187, 44, 79,188,158,
187, 46,116,194, 20, 22, 79,123, 80,129, 77,143, 79, 77,143,123, 79, 71,129,128,
 79,178,194,193,185,179, 74, 32, 81,194,180,185,192, 26,123, 26, 79,129, 77,125,
 70,125, 77,143,129, 44, 26, 26, 22, 79,123, 80, 20, 38, 32, 26,123, 18,  4, 22,
 28,158, 74, 32,194, 81,180,192,178, 86,180, 38,193,192,194,178,128, 79,178,193,
194,185,179, 73, 77,123, 80,125,129,183,125,124, 26,194,187,193,192,179, 74, 32,
192, 81,130,187,194,128, 79,178, 30,194,193,187,106, 79, 30, 80, 85, 83, 32,163,
 22, 79,116, 32, 44, 80, 79,116, 77,188, 79, 44, 46, 86,180, 72,123, 79, 77, 44,
129, 80, 22, 81, 32,129,125, 77, 80,  4,123, 14, 77, 32, 80, 81, 87, 79, 80,129,
 29, 25, 85,  2, 77, 32,123, 80, 81, 30, 26, 79,129, 14, 30,123, 80, 79,128,180,
178, 30,194,179,193, 79,129,125,172,158, 20,174, 16,125,180,129,123,172,143,150,
125,123, 80, 81, 30, 77,155,129, 81, 80,125,192,158,148,192,172,174, 78,129,123,
123, 22,136, 71,180,128, 79, 85, 79,136, 79, 20, 26, 77, 32, 72, 57,187, 32,180,
 77, 81,  2, 80,136, 77, 71, 83, 85, 28,136, 80,180, 81, 79, 85, 26,174,180,136,
 85,128, 80,143, 16, 83, 77, 81,180, 85, 79, 65, 77,188,128, 71,134, 80, 26,128,
 79, 80, 85,180, 81, 72, 57, 82,128,134,188,138, 28,194,193, 80, 79, 71, 82, 34,
 77,174,194,128,180, 83, 81,124,194,193,192,255,255,255,128, 79,194,193, 30,178,
255,130, 87, 67,180, 80, 63, 68, 79,125,180, 80, 46,123,129, 18,180,143, 79,123,
125, 74, 72,123, 74, 79, 78, 77, 83,129,128, 79,194,187, 30,193,179, 22,165,116,
 79,180,194, 71,124,194,124, 26,192,193,179, 16,116,180,120, 92, 80, 79, 85,116,
172,180,158,188, 79, 79,188,158,187, 46,116,194, 16,129,128, 30, 79,193,194,255,
255,124, 26,194,192,124,193,255, 60,192,180,194, 32,116,255,120, 22,116, 77, 80,
 79,120,  8, 32, 71,116, 20, 26, 30,154,165,192, 77, 26,193, 79, 77, 60,192,194,
 81,178, 32,180,128, 30, 79,194,193,255,255,  8,129,125,123, 81, 26, 20, 61,125,
 32, 56, 82,123, 26, 62,125, 32, 20, 46, 81, 26,148, 81,123, 78,129,143, 83,125,
 60, 81,194, 32,192,130,178,128, 30, 79,194,193,255,255,168, 71, 81, 79, 85, 80,
 57,  8, 81,193,116, 82, 77, 80, 22,116, 26, 81, 77, 83, 80,124,194,192, 26,193,
255,255,123,128, 30,180, 79,193,128,194, 61, 77, 82,136, 26, 81,170, 62, 83,136,
118,132, 20, 82,124, 26,180,193,194,192,255, 60, 81, 71,178,188,174,180, 22, 71,
193, 77, 26,136, 81, 71, 60, 74,123,192,180,194, 32, 61,125, 32, 73, 26,143,178,
  8,123,129, 73, 72,143, 46,  0,123,129,194, 73,125,180, 62,125, 76, 20, 32, 74,
180,128, 30,194,193,255,255,255, 79, 60, 81,194, 32,178,180,192, 61, 82, 81, 32,
125,123, 26, 62,125, 77, 81, 20, 83, 82,124, 26,192,193,194,255,255,128,178,180,
 30,194,193,255,120,125, 77,129,123, 81, 72,150,123,158, 83, 81,180, 79, 71, 57,
166,132, 82, 64,108, 79, 81,124,192,180, 26,194,187,179,152, 50,108, 30, 82, 83,
 80,128, 30, 79,178,180,193,128, 18, 57, 50, 71, 92, 77, 83, 50, 51,123,125,143,
180, 71, 64,100,125,123, 71,129, 30, 85,  2,123,125, 71, 32, 64, 78,255,255,255,
255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,125,168,
 71, 85, 79, 81, 80, 83,152, 50, 82,116, 71, 80, 81,166, 71,130, 81, 80, 83, 79,
124, 26,180,192,194,187,193,153, 71,188, 77, 83, 50,116,  2,110, 71, 80,116, 81,
 79, 30, 58,123, 64, 50, 77, 71, 57, 64,180,123, 79, 71,125, 18,  2,123, 79,125,
183, 85, 57, 51, 79,123,125,143, 18, 24, 34,180,123,183, 79, 50,125, 16, 50,123,
 79,143,125, 46, 79,124, 26,194,192,180,193,179,128, 30,180,193,194,178,179, 79,
129, 66, 20,143, 32,123, 58, 30, 72,123, 26,129,125, 26,123, 72,129, 26, 32,143,
  4,123, 77, 32, 81, 20, 26, 71, 79,129,125, 50, 74,123,143,  4, 50,123,125, 73,
 74, 22, 58, 75,123,125,129, 72, 30, 16, 50,123,125, 73, 76, 75,124, 75, 26,180,
194,192,179,149, 72,129, 30,143, 75, 76,

79,
129,115, 67,128,194,187,180,179, 30,193, 22, 74,116, 69, 60,124,187,148,114, 53,
 74, 60, 69, 26,  2,116,120, 60,187, 81,124, 28,116, 60, 74, 53, 68, 69, 16,120,
 81, 74, 69,188, 53, 83,128,194,193,180,187,179, 30,124, 26,124,194,192,193,187,
162, 69, 80, 55, 97, 76, 81,  2,116, 81,120, 69, 62, 55,170,158, 82, 76, 69,187,
188,154, 81, 80, 69, 82, 46, 55,180, 80,194,115,133,143,137,166,128,124,116, 80,
 60,120, 74, 18,116, 67,120,187, 80, 44, 26, 32, 80,120, 60, 46,174,162, 95, 44,
 67, 53, 46, 88, 72, 74,116,120, 67, 60, 53, 82,128,194,193,187, 30,179,255, 26,
 81,116, 83,120,187, 80,  2, 80,116, 81, 61,120, 54,124, 26, 75,124,194,187,192,
162, 75, 80, 96, 81, 54, 83,  4, 81, 80,116, 61, 89, 68, 74,128,194,193,180,179,
187, 30, 72,120,116,187,180, 32, 30, 18, 71, 73, 72,120, 26, 67,124, 26,124, 75,
180,179,193,  4,120,116,187, 73, 71, 32, 28, 67,187,116, 73, 26, 60,120, 22,113,
117,123, 60,103,121,148,129, 53, 74,135,188,133,120,102, 32,188,180, 80,124,155,
115,129, 74,135,180,174,163,115,129,135,133,180,105,150, 53,115, 80,129, 67,135,
162, 32,115, 85, 26, 20,180, 24,188,119,180,188, 85,187,174,194,121, 85,187,188,
 80,180, 26, 22, 80, 26, 38,188, 24,180, 86,192,180, 16, 80,174, 26, 28,180,174,
 85,187, 26, 16, 67,154, 66,116, 74, 65, 60, 53, 65, 30, 66,180, 22, 32, 26,115,
 66, 69,194,116, 20,180,168, 74, 26, 65, 44,188,187,170,116, 74, 65, 81, 26, 46,
 16, 65, 66,116,172,180, 69, 46,115,194,180, 67, 74, 40, 60, 16,144,174,194, 67,
188, 38,154, 74, 38,194, 67,144,188,101, 67,144, 38, 74,188,194,176,188,142, 53,
194, 83,174, 86,192,194,180, 38,255,255,194,115, 67, 95, 46,180, 44,166, 16,188,
 67, 95, 32, 53, 46,154, 32, 74,188,180, 95, 67,101,188, 32, 46, 95, 44, 74, 65,
 67, 32, 46, 44, 30,188, 18, 32, 46, 67, 74, 95, 44, 80,121,115, 66,143, 87,187,
 73,154, 87,117, 79,135,143, 32,115, 87,115, 79,143,135, 66, 22, 32,143,135, 79,
 81, 87, 86,143,125,145,139, 87, 32,  4, 79, 87,135,143, 32, 81, 74, 72, 73, 67,
187,188,194,120,115, 71, 73, 67, 44, 46,180,101, 71, 73, 67, 26, 32, 75,  4, 73,
 67, 26, 44, 71, 32, 58, 71, 30, 73, 22, 75, 76, 86, 71, 73, 67,187, 53,194,119,
120,148, 53,129, 74,135,133,105,155,115,135,129, 74,174,133, 22,123,117,103,113,
121, 80,163,115,135,129,133,174,105,149,115, 53, 80,188, 82,135,150, 53, 67, 74,
115,129,135, 74,128,180, 30,179,193,194,187, 22,116, 60, 72,174, 71,188, 28,116,
 72,120, 73, 60,174, 72, 30, 73,180,188,120,174, 18, 72,120,174, 26, 44, 46,155,
 32,188, 71,174, 72, 76,116, 18,123,117,121,113,103, 80,148,121, 67,131,129, 82,
 83,155, 32,121,188,174, 67,187,164,121,101,129,131,125,111,149,121, 83,131,188,
 80, 67,163, 67, 80,174,101,188,129,144,114,131,129, 67,135, 80, 74, 65,131,135,
 14, 82, 30,139, 16,129,131,135, 60,187, 44, 78,131, 80,194, 32, 14, 82,128, 79,
 30,194,187,178,255,148,131,129, 74, 67, 46,135, 80, 93,125,117,115, 77,143,187,
154, 87,115, 79,135,143, 32, 77,117,125,143,174,194, 46,148, 66,121, 81, 79,143,
 73,128, 79, 30,194,187,193,255,114,125,117,121,115,135, 81,124,114, 67,111,129,
 80, 74, 44,128, 79, 30,193,194,187,178, 72,109, 30, 74, 77, 14, 80, 65,174,115,
109, 30, 46,111, 26,123,117,113, 80,121, 60, 16,129, 67, 60, 53, 80, 46,121, 74,
 72, 30,180,193,179,194,187, 16, 73, 67, 76, 72,120,116, 18, 72, 73, 75,120, 44,
 46,108,174, 72,193,180, 44, 46,155, 72, 32, 67,174,116, 60,149, 53, 72, 75,188,
174,187,116, 18,121, 80,117,103, 32,174,148,121, 80,187, 82, 44, 74,162,121, 82,
 67, 80,129,187,116, 82, 74,112, 44, 46, 32,155,121,187,188, 32, 80, 67,164,129,
121, 80, 82,131,101, 60, 72, 30,194,187,193,179,255,108,174,187,193, 74,192,165,
116,116,120, 67, 53,134, 62,168,188,134, 81, 61, 67, 74,148, 53, 59, 61,114, 67,
187,149, 53, 74,188,134, 67, 44, 67, 16,116, 69, 68, 65, 53, 81, 72, 30,194,187,
193,179,255,108, 65,116,187,193, 74,174,149, 53, 65,188,174, 74, 68,126, 65,120,
174,116,194, 46,154,120, 66,114, 74,187,193,187, 72, 67,194, 32, 60,152,116, 18,
120,166,195, 32, 80,116, 80,137,123,159, 67,133,152,116,194,166,120,159, 46,144,
 16,166,152,116,120,173, 80,162, 67, 53, 74, 88, 95,159, 82, 72, 30,194,187,193,
179,255, 26, 80, 89,120,116, 26,187,108,187, 80,188, 75,179, 81, 18, 54, 75, 61,
 68,120, 81,154, 61, 54, 80, 68, 75, 83, 16, 68, 54, 80, 61,116, 83, 18, 67, 24,
120, 65,174, 60, 81,194,121,120,116, 60, 65, 53, 68, 48,120, 65,174,116, 81, 60,
 16,116,120, 69, 65, 68, 32, 26, 65,120, 63, 69,116, 68,115,180, 69, 60, 53, 74,
 68,120,148, 80, 67,129,105, 32, 83,157, 32, 80, 74,129,135,133,163,105,129,135,
 74,174,133,150, 67, 32,129, 80,105,188,149, 74, 82,105, 83,188, 80,255,255,255,
255,255,255,255, 32,121, 24,187,120, 38, 40, 26, 86,120,187,174,194,193, 38,115,
188,194,120, 94, 20, 26, 10,120, 85,194,116, 24, 84, 24,120, 38, 24,116, 85, 86,
158,114,194,188,180, 38,192, 74, 30,120,116,174, 26,180, 44,115, 71, 73, 72,120,
 26, 67,121, 72, 73, 75,120, 44, 46, 24,120, 60,174, 73, 81, 67, 72, 73,120, 67,
 75,180, 22,119, 72,120,174, 26, 44, 46,187,121,120,166,195, 32, 80,116, 65,120,
 30,194,144, 22,124,101,166,194,144,195, 80, 82,162, 32,116,166,124,142,144,148,
114, 32,166, 82, 46,195, 72,116,166,194, 32,120, 82,194,115, 32,187, 95,166,120,
 88,121,166, 46,195, 82,120,187,148, 95, 83, 32, 30, 82,166,149, 83,120, 74, 82,
180,187, 72, 95,166, 30, 82, 46, 32,101, 67,120, 32,144, 46, 44, 86,174, 65, 67,
120, 30,160, 95, 74, 72, 74,166,167,124,255,255, 18, 74, 95,166, 67,167, 82,121,
153, 67,160,158, 95,195,162,124,116, 67,192,180, 32,101,166, 67, 74,120,160, 53,
116, 18,103,117,113,123, 80, 32,148,129,131,101, 67, 83, 79,164, 80,121,131,129,
101,125,163, 32,174,180,101,129, 67,149, 67,121, 77, 82, 83,180,255,255,255,255,
255,255,255, 74, 72, 30, 73, 67,116,120, 53, 26, 72,120, 81,180, 75, 60, 22, 72,
 60,174,180, 71, 22, 85, 72,120, 75, 76, 73, 71,115, 72,180,174,116,120,134,121,
120, 72,116,180, 60,174, 83,121,158,120,116, 62,174, 80,162,192, 69, 80, 97, 55,
194,101,144, 81, 76, 62, 55, 97,170, 81,180, 80, 69, 82,194,  2,120, 76,116, 97,
 82, 62, 22,116, 62,193,194, 80,144, 53,101, 74, 54, 81,144, 60,158,162, 67,194,
 51, 49, 54, 55,121,116,158, 60,120, 74,187, 72, 74, 67, 49, 60, 54, 81,164, 67,
 55, 81, 74, 52, 60,170, 74,180, 51, 67, 81, 49, 60,162, 67,180, 53,194, 61, 62,
 65,120,116,114, 67, 30, 74,121,120,187,116, 62, 74, 61,115,180,194,120,116,187,
188,148,114,192, 53, 67, 81, 61,101,120, 74, 61,124, 81,144,125,121,116, 18,121,
 80,117,103, 32,174,148,121, 80,187, 82, 44, 74,162,121, 82, 67, 80,129,187,116,
 82, 74,112, 44, 46, 32,155,121,187,188, 32, 80, 67,164,129,121, 80, 82,131,101,
 80, 18, 82,187,138, 20, 26, 44,168, 87,138,158,110,116, 20, 34,151,116, 26, 20,
 44, 46, 93,116, 87,187, 73,158, 77,154,130, 87,110,138,187, 32,108,187,116,194,
 26, 46,110, 82, 26,116, 26, 80, 83, 89, 79, 81, 81,130,187,194,192,255, 18,116,
 61, 80,188,187, 75,168, 80, 79, 26, 20, 44,116,108,187, 80,192,193,255,255,  2,
 61, 79, 80, 89, 75, 68,187, 72,194,152,116, 83, 30,166,168,130, 32,116,110, 95,
 80, 80,159, 22, 67,116, 60, 83, 16,116,152,166,173,110, 44, 22,116, 32, 60, 82,
130, 79,162, 32,159, 80,130,188,116, 44, 18, 80,187, 38, 74, 82, 83, 86, 38,194,
193,192,178,255,108, 80,192,193,187,185,255, 81,194, 32,130,192,187,255, 93,116,
 74, 80, 79, 38, 67,168, 80,116, 53, 83, 74, 82, 46, 86, 38,194,178,192,255,255,
 18,194, 80,187, 38, 83, 74,108, 80,187,185,192,255,255, 81,130,194,192,187,255,
255, 93,116,194, 80, 79, 74, 30, 78,194, 79,116,187, 80,184,115, 80,124, 26,179,
194,187,193,192,148,138,110,116,188,158, 26, 22,116, 79, 81,187, 87, 46, 16,188,
116, 73, 83, 46, 79,162,116,138, 79, 87, 66,110,155,116,110,130,188,158, 32,187,
168, 74,116,166,188,130,110,154, 32, 60,116, 67, 46,166, 72, 74,116, 77, 67, 78,
 30,120,152, 79,194,166,159,116, 26,116, 80, 32, 79, 82, 53,162, 32,116,166, 67,
110,130, 67, 81, 81,187,194,192,130,255,124, 26,194,193,187,192,255,168,130, 66,
 74,116, 46,187,148,187, 53, 32,110, 68, 74,128,194,180,187,193,179, 30,155,130,
 66, 53,110, 68, 74, 46,124,194,187,185,192,255,255, 18, 80,187, 38,138,180, 82,
155,194,116, 80,187,188, 40,168, 80,116, 53, 74,188,110,148,116,194, 80, 53,187,
 40, 86,180, 38,192,194,178,255,138, 18, 80,143,123,125, 46, 74,124,194,193,192,
255,255,255, 72,123, 74, 79, 78, 77, 83, 78, 79, 80,129,123, 74,180, 77, 79, 80,
 78, 46,123, 82,168,129, 74, 82,143, 44, 46,180,162,166,130, 80, 44,116,110,  2,
187,116, 46, 80,152, 44, 18,187,166, 80,110,152, 44,148,116,110,187, 80,166, 44,
 26,116, 80,110, 44, 30,187, 80,194,110,187,116,166,152,119,116, 18,123,117,121,
113,103, 80,148,121, 67,131,129, 82, 83,155, 32,121,188,174, 67,187,164,121,101,
129,131,125,111,149,121, 83,131,188, 80, 67,163, 67, 80,174,101,188,129, 46,114,
194, 38, 82, 80, 53, 67, 86,194, 38,178,192,255,255,  2,194,187,138, 60, 82, 38,
 65, 67,194, 38, 82, 30, 78, 78,194, 67, 82, 38,130, 53, 26, 60,194, 38,187,138,
 40, 80,154,130,116,138, 87,180,110,114,116, 81, 79, 82, 46, 73,148, 73, 81,116,
130,138,110,149,180,188,116, 73,130, 81,162, 87,130,138,116, 32, 44, 65, 81, 78,
 73, 82, 77, 83, 83,128, 30,194,187,193,178,179,148,130, 81, 80,116,138,110,155,
116,130,110, 80, 82, 32,  2, 80,116, 82, 81, 46,187,163, 80, 81,138,158, 32,116,
 82,116, 82, 32,188,130,138, 82,  2, 80, 61, 46, 81,187, 83, 26, 81,116, 80,187,
 83, 26, 18, 80, 81,187,188,194, 46, 65, 81, 75, 68, 78, 32, 80,114,116, 54, 80,
194, 81, 79, 72,116, 68, 54, 32, 78, 75, 44,114, 38, 67, 80, 82,116,194, 65, 38,
116, 36, 77, 78,138, 86, 38,192,194,193,178,255,  2, 38, 60,194, 28, 82, 80,155,
180,188,130,116,194,110,148, 38,130, 53,110,116,138, 72,116, 18,117,103,123, 80,
 22,121,162,188, 82, 79, 77, 46,103,148,101,129,131, 74, 67, 79,149, 67,131,121,
129, 80, 32,164, 80, 30,131,101,129,121,163, 67,180,101, 30,129, 44, 79, 16, 32,
 81, 80,138,194,116,148,116,138, 32, 81, 80, 20, 65,194, 32,154, 81,130, 26,115,
116, 86,158, 26, 20,194, 22,116, 72,192, 81,130, 78, 28,116, 32, 72, 81, 82, 80,
 67,115,116, 26, 20, 74, 30,187, 74,180, 74, 32,130,187,178, 16, 32, 74,130,138,
 81, 30,148,130,138, 74, 65,110,116,101, 63, 74,130, 68, 81, 69,  2, 63,130, 74,
116, 30, 60, 80, 74, 32, 81,192,130,180,194, 75,116, 87, 77, 81, 46, 30, 71,116,
 73, 79, 78, 87,130,168, 73,130, 32, 79, 87, 44,148,116,130,138, 87, 81, 79, 65,
116, 87, 30, 73, 81, 77, 53,  2,130,116, 74, 60, 32, 26,162, 81, 49,130,138, 51,
116,101,130, 74, 81, 67, 54, 60, 16, 74,130, 81,138, 44,116, 74, 74,180, 32,178,
130,192,121,116, 60, 74, 81,130,187, 60,168, 53, 74, 67, 81,110,130,162, 81,130,
 30, 32, 26, 53,148,116,138,110, 74, 61, 67,101, 74, 81, 67, 61, 53, 62, 74,178,
 74, 32,180,130,187,115,116, 26, 20, 74, 61, 32, 65,116, 67,123,127, 22,109,129,
 32, 18,123,117,113, 80, 32,103,148,131,121,129, 79, 74, 82,164,121,125,131,111,
129,101,149,121, 67, 30, 74, 79, 77,255,255,255,255,255,255,255, 30,121,116,130,
 79, 74,184, 24,115,116,180, 79,187, 67,138,119,116, 38,110, 14, 83, 80, 63, 38,
 36, 79,184, 82, 80, 72,116, 80, 74, 79,130, 60, 66,116, 24, 79, 80, 36,188, 74,
 72,116, 30, 73, 67, 53, 60,121,116, 72,138, 73, 26, 30, 66,116, 20,130,192, 75,
 32, 18, 76, 72, 67, 73,188, 22, 68, 26,116, 22, 75,138,188, 34,116, 76, 73, 60,
 53, 72,130, 69, 32,117,125, 75, 14, 46,119,121, 72, 75,125, 73,117, 16,117,125,
135, 32, 69,193, 67, 72,125,192, 75, 71, 73, 68, 72, 32,125, 73, 22, 14,115,121,
 72,180,117, 75, 71, 32,115,116,188,194,180, 86, 73,121,116, 73, 86, 84,130, 85,
119, 26, 86, 38, 40, 73, 24, 86,116,154, 26, 40, 48, 20,  2,116,130, 86, 38, 73,
 40, 16,116, 38,130, 73, 26, 40, 67,121,130,193, 74,187, 81,138,119, 46,187,130,
116, 81, 68, 66, 74,130,193,116, 81,192,148,130, 74,138, 30,110, 69,101,130,110,
 74, 81, 60, 53,  2,138, 74,130,179, 60, 69,168,130, 26, 55,188, 97, 62,180,115,
 22, 73, 75, 55, 97,187,115,176, 32, 74,154, 72, 55, 69,121, 72, 73, 55,121, 74,
 75,175,162, 74, 73, 75, 69, 72,161, 69, 32, 74, 75,188, 72,188,121, 32,130,187,
 80,116, 53,154,116,130, 32,110,138, 46, 22, 95,130, 53,138, 32,116, 72, 74,116,
130, 79, 82,110, 80,130,195, 32,194, 74, 67, 18,130,116, 32, 74, 80,110,116,115,
129, 67, 74,180,188, 46,121, 32,121,129, 67, 80,187, 16,188,129, 67, 74,121,180,
 22, 32,129,121, 67,180, 80, 18,123, 74,113, 32,188, 99, 28,129, 74,188, 67,121,
 32, 32, 86,116, 59,180, 20, 16, 40,115,188, 73, 84, 66,130, 80,147,116, 59,188,
138, 85,180,121,116, 84,130,187,188, 80, 77,138,130,116, 80, 38, 86, 28,130,116,
 73,138, 38,187, 74,121, 72,130, 32, 26, 44,116, 22,130, 53, 73,187,110,116, 72,
130,110,188, 76, 73,187,176,130, 81,116,138, 26,110,175,130,116, 81, 72,162, 44,
161, 67,130,116, 75,187, 73, 53,176,130, 81, 32, 26, 20,138, 16,130,188, 74, 32,
 44, 46, 51,130, 74,188, 81, 67, 32,182, 81,130, 51,116,138, 52, 83, 74, 46,138,
187, 67, 32, 26,130, 51,188, 74, 32, 46, 80,121,123,154,136, 87,187,114,110,118,
 72, 79,180, 30,128,193,179,149,118, 20, 44,174,187,138,155,136,174,110, 32,188,
138,168, 87,136, 32,187, 26, 44, 18,174,136,188, 59,187, 87,158,108, 66,159,165,
151,255,255,149, 66, 30, 26,143,166, 59,148, 32, 26,123, 59, 73, 87,168,123, 26,
 32, 59,129,125,162,123,143, 87, 66,125, 46,106,123, 59,125, 20, 26, 30,129,108,
138,193, 75,151,174,165, 72, 30,194,193,179,255,255, 18, 54, 75, 82, 61, 68, 90,
154, 61, 54,138, 68, 75, 46,126,116,120,194, 46,138,158, 16, 68, 54, 82, 61,116,
120,125, 18, 82,187,138, 20, 26, 44,168, 87,138,158,110,116, 20, 34,151,116, 26,
 20, 44, 46, 93,116, 87,187, 73,158, 77,154,130, 87,110,138,187, 32,108,187,116,
194, 26, 46,110, 87, 72,194,193,179,255,255,255,108, 80,165,174,193,151,192, 16,
125,129, 25, 84,123,143, 28,125, 25,143,123, 80, 85, 22,125, 84, 25,129,143, 80,
150,123, 84, 85, 86,143, 19, 20, 18,123, 38,125, 44,143, 26, 86, 38, 87,194,193,
192,255, 16,123,125, 79, 73, 38,143, 22,123,125, 38, 79, 73, 26, 28,123,125, 38,
 26, 44,143,162, 38, 66,123, 32, 26, 44, 72, 73,115,180, 30,123,129, 32,143, 79,
129, 72, 80,123,158, 87,148,123,129, 75, 30, 76, 80,  2,129,123, 72, 30, 87, 74,
119,123,129,125,174, 74,143, 16,129,123,125, 74, 30, 76,125, 74, 32, 81,192,130,
180,194, 75,116, 87, 77, 81, 46, 30, 71,116, 73, 79, 78, 87,130,168, 73,130, 32,
 79, 87, 44,148,116,130,138, 87, 81, 79, 65,116, 87, 30, 73, 81, 77, 79,119, 72,
194, 86,143, 80,193,148,129, 72, 83, 80, 82,123,115, 20,125, 86, 72,180,158, 22,
123, 72,129,125, 83,143, 16,123,129,125, 83, 32, 26, 28,129,123,125, 32, 72, 26,
 87,115, 37, 86,123,129, 19, 43, 22,123,125, 86,129, 84, 85, 26,123, 86,194,129,
 84, 37,119,123, 86,129, 94, 37, 84,149, 80, 84,123,125, 86, 19,148,123,129, 23,
 80, 37,125,129,115,116, 44, 75, 54, 32, 68, 75, 96, 32,116,192,120, 44, 74,180,
 32,194,192,255,255,119, 61, 54,194, 68,116, 75, 16, 96, 68, 32, 30,192, 54,101,
 68, 96, 75, 61, 82,194, 30,121, 38,158,123, 79,143, 22,115, 73, 24,123,125, 18,
 82,119,123,125, 73, 83,143, 38, 74,180,194, 81,192,185,255, 16,123,129,125, 81,
 73, 83, 22,123,125, 79, 73, 24, 38, 65,123,121,136, 73, 77, 30, 20, 26,115,136,
 79, 77, 73,188, 83, 66, 73, 79, 30, 87,136, 83, 67,174, 73, 81, 30, 79, 78,119,
 73,174, 30,136, 79, 81, 69, 77, 79, 83,136, 87, 73, 73, 67,123, 32,192,194, 74,
178, 79,129, 72, 80,123,158, 87,121,123, 26, 74,125,129, 30,115,129,125,123, 30,
 72, 32,119,123,129, 74,194,125, 30, 69,125, 32, 22, 46,129, 76, 30,115,129, 79,
 73, 24, 81,123,121,123, 79, 73,125,129, 38, 72,125, 73,129, 24, 79,143,119,123,
194, 73,129,184, 81, 79, 24,129, 40,123, 79, 22, 67,123,194, 81,192, 18,185,129,
 67,116,194, 32,192,255,255, 26, 26, 30, 82, 75,174, 96,162, 68, 75, 26,138, 96,
 46, 69, 68,120, 75, 44, 61, 54,101, 68, 96, 75,138, 61,194,121, 68, 54, 75,120,
 30, 26, 87,119, 86,194, 94, 25, 43, 85,115, 25,180, 86,129, 23,123,149, 80, 86,
 84,123,125, 25,150,123, 86, 94, 37,192,143, 79, 84, 85,125,129, 86, 80, 86, 25,
174, 37,158, 80, 15,125,121, 73, 87, 32, 26,130, 20,148,138,110, 87, 81, 82, 83,
 72,116, 30, 73, 79,130, 83, 69, 87, 73,192, 32, 79, 77,119, 81, 78, 73, 82, 77,
 83,149,116, 82, 81, 73,138, 78, 86,129,115,158,116,180,174,120,138,119,174,194,
138,116,158,120,121,116,158, 61,120,174, 75,162,138, 68, 96, 54,194, 75, 22, 61,
174,194,144,116,138,101, 75, 61, 54,194, 96, 82,125, 72,116, 30, 73, 79,130, 83,
148,110, 79,130,138, 81, 82, 65,116, 87, 30, 73, 81, 77,121,158, 73,116,138,130,
110,101, 79,130, 78, 81, 73,158,  2,116, 73, 82, 78, 79, 77,158,115,125, 79,152,
129,166, 77, 65,125, 59, 66, 30, 26,176, 72, 73, 79, 59, 20, 26, 32, 79,123, 59,
125, 20, 26, 30,101, 59,143,125, 79, 87, 26, 93,123, 59, 79,129, 30,152,143, 22,
194, 73,130,128,174,138, 65,128,138, 30, 87, 73,130, 72, 73,130, 79, 30,128, 22,
115,180,194,128, 77, 79, 73,148, 87,134, 79,194, 81,130,119,194, 79,174,158, 87,
130,123,121,174,136, 73, 79, 83,180,115,136,174, 77,128,180, 87,119,136,180,174,
 81, 87,188, 72, 73,180, 79, 87,188, 77, 65,136, 77, 73, 79, 30, 78, 93,136, 73,
188, 79, 77, 30, 87,119,123, 25,174,194,193, 23,115,180,123, 25,143,158,194,121,
158, 25, 80,123,129, 19,149,129, 37, 80,180,125,143,101,143,123, 25,194, 19,129,
 79, 84, 85,125,129, 86, 80,148, 79, 73,123, 83, 81, 80, 82, 78, 59,129, 80,193,
192,179,255, 83,123, 46, 32, 78, 80, 81,154, 77,123, 78, 65, 80, 32, 66,123, 65,
 83, 82, 80,129,164,123, 80,125,143, 77, 30, 87, 72,123,129, 23, 80, 37,125,149,
 80, 84,123,129,125,143,153,129, 80,123,143, 84, 85,121, 85,123,129,158, 86, 84,
119,174,123, 25, 85, 94, 80,152,123,194, 86, 80, 25,125,123,115,108,114, 26, 87,
136,187,121,136, 87,114,108,188,174,152,136,118, 82, 79, 83, 20,154, 87,187,114,
136, 79, 32,151, 20, 73, 83, 79, 26,136, 72, 73,108,188, 78, 83,114, 46,115,194,
 38,129,158,125, 40,121,194,158, 38,123,129, 40, 72, 38,123, 81,194, 83, 73, 58,
123,129, 38,194, 30, 83,101, 38,123,129, 79,194, 87,  2, 79,123, 38,129,194, 73,
129,154, 75,138, 61, 54, 68,180,119, 75,114,194, 46, 82, 54, 51, 75, 68,138, 61,
 54, 82,115, 96, 54, 68,114,138, 75,121, 54, 75, 61, 68,114,138,101, 75, 96, 54,
 61,138, 82,143,115,138,158,128,194,180, 79, 72,130,138, 83,134,128, 82,121,158,
138,194,187, 87, 20, 65,138,130, 83,128,134, 87,119,194, 81, 82, 26,130,187, 58,
130,138, 87,128,134, 81, 16,129,121, 68, 54, 82, 61,116,120,162,138, 75, 96, 46,
 82,180, 72, 96, 68, 32, 30,192, 54,101, 96, 68, 61, 54, 75, 82,119,120, 75,174,
116, 82, 54, 86, 61, 54, 68, 96, 82,116, 79, 73,129,125,123, 81, 72, 86, 81, 81,
194,192,255,255,255, 83,123, 82, 78, 77, 83, 46,  8,123,125, 80,194, 46, 86,150,
123,125, 78, 80,143, 77, 82, 65, 32,129,123, 81,194,123,115,136, 20,118,132, 73,
 32,121,136, 59,174,188, 87, 20,158, 59, 73,114, 87, 79,136,168,114, 87, 73,188,
136,174, 10, 79,136,174,188,180, 82,162, 87, 79,108,180,138,136,125,121,116, 87,
 26, 20, 44, 46,115,188,116, 73, 83, 46, 79,168, 87,130,110,188,138, 73, 65, 73,
116, 81, 30, 82, 87, 24, 79,116, 32, 87, 81,130, 48,116, 79, 81,138, 87, 44, 87,
115, 15, 25,143,125,172, 80,121,125,129, 25, 84,123,143, 24,174,129, 86, 15, 25,
123, 10, 84, 85,125,129, 86, 80, 72,129, 23,125,123, 37, 84, 86, 25,174,192,125,
 37,129, 73,121,129, 26, 20,123, 80,143, 10,129, 72, 80,123,158, 87,  8,125, 74,
 72, 80, 76, 46, 72,129,123,125, 74, 30, 76,115,129,125,123,143, 20, 80,119,129,
123,125, 75, 44, 26, 82,121, 80, 18,123,125,129, 87, 20, 44, 16,129,125,123, 87,
143, 83, 72, 79, 30,193,194,179,255, 26,123,129,125,143,174, 87, 78,123, 87,125,
143,129, 79, 28,123,129,125,143, 87, 26,129, 72, 30,194,187,193,179,255, 26, 80,
 89,120,116, 26,187,108,187, 80,188, 75,179, 81, 18, 54, 75, 61, 68,120, 81,154,
 61, 54, 80, 68, 75, 83, 16, 68, 54, 80, 61,116, 83,125, 26,116, 26, 80, 83, 89,
 79, 81, 81,130,187,194,192,255, 18,116, 61, 80,188,187, 75,168, 80, 79, 26, 20,
 44,116,108,187, 80,192,193,255,255,  2, 61, 79, 80, 89, 75, 68,123,155, 80, 79,
 81,136,138, 32, 26, 80,136, 79,118, 83, 44,168, 80,136,187, 79, 20, 44,154, 80,
136,187,114, 79,132,108, 80,187,193,192,255,255, 18,136, 80, 61, 79, 26, 20,143,
 81, 81,187,130,194,255,255, 26, 80, 83,134, 44,130, 32, 77, 80,128, 79, 78,134,
 75, 78, 80, 79,128,134, 30, 75, 86, 80, 75, 79, 61,128, 83,108, 80,187,193,255,
255,255, 79, 81, 81,192,194,255,255,255, 18,123,129, 32, 26, 20, 44, 26,123,129,
 77, 26, 44,125, 75,129, 32,125,193,123, 44, 83, 26, 20,123,125,143, 46, 28,125,
123,129, 32, 26,143,119,123,155,188, 80, 79, 61, 46,110, 72,136, 61, 77, 81, 30,
 46, 86,136, 80, 81, 78, 77,128, 65,136, 79, 81, 61, 78, 80,114, 80, 79,138,136,
 68, 46, 93,136, 81, 30, 80, 77, 20,125,  2, 80, 61, 46, 81,187, 83, 26, 81,116,
 80,187, 83, 26, 18, 80, 81,187,188,194, 46, 65, 81, 75, 68, 78, 32, 80,114,116,
 54, 80,194, 81, 79, 72,116, 68, 54, 32, 78, 75, 80, 72,125, 73,129, 30,123, 78,
114,123,125, 26,143, 20, 44,149,123,143,194,125, 81,180,128, 79, 30,194,193,179,
255, 65,123,194,125, 87, 81, 73,150,123,125, 81,143, 73, 82, 81,128, 79, 30,194,
193,178,255,114,125,129, 74, 80, 82, 83,106,129,125, 80, 82,123, 74,134,123,125,
 80,129, 74,174, 65,123, 74,125, 78, 77, 30,155,174,194, 32,180,143, 67,143,114,
 80,128, 79,138, 44, 81, 65, 32,194,138, 14, 75, 30, 93, 75, 79, 30, 14, 77, 68,
 77, 80, 79,128, 32, 81, 78, 72, 32, 79, 30, 61, 80, 68, 86, 79,194, 75, 81, 78,
 77, 83,128, 30,194,193,178,179,255, 72,125,129, 30, 80, 78, 82,106,129, 80, 82,
123,125, 76,114,129,125, 46, 80, 79, 82, 86,194, 80,143, 81,123, 82, 65, 81,123,
125, 78, 77,129, 72,129, 74, 32, 81,194,192,187,255,119,116, 80, 61, 54, 68, 75,
 75, 75, 96,120, 32,116,124,101, 80, 68, 96, 75, 83, 81,  4, 80,120, 68, 81, 75,
 54,  2, 68, 61, 96, 83, 75, 80, 79,119, 72,194, 86,143, 80,193,148,129, 72, 83,
 80, 82,123,115, 20,125, 86, 72,180,158, 22,123, 72,129,125, 83,143, 16,123,129,
125, 83, 32, 26, 28,129,123,125, 32, 72, 26,125, 74, 32, 81,194,130,192,178,168,
 79, 75, 26, 96,116, 81,  2, 61, 30, 79, 83, 44, 80,  4, 61, 80, 30, 81, 79, 77,
 75, 75, 77,116,130, 81, 96,119,116, 68, 54, 32, 78, 75, 77, 16,123, 79, 83,129,
 26,125, 26,129,123, 79, 83, 26, 81,119,123,129,125, 80, 30, 78,115, 56,123, 26,
 44,129, 20, 22,129,123,125, 22, 83, 81, 70,129, 79,125, 30, 26, 20, 89, 74, 32,
192,178,194,255,255,119,122,194,136,116, 46, 90, 16,116, 96,194,136, 90, 32, 22,
116, 22,144,136,122, 82, 75,116,194,192,122, 32, 46,148, 82,116,118, 90,136,122,
123, 74, 32, 81,130,178,194,187,115,188, 77, 81, 78, 22, 20,119,136, 61, 77, 81,
 30, 46, 73, 79, 77, 61, 68, 80, 81, 26, 83, 79, 77, 81, 26, 78,101, 77, 61, 79,
136, 81, 83, 26, 80,121,123,129,125,143,174, 87, 86,174, 79, 87, 73, 26, 81, 93,
 79, 26,129, 77, 81, 87,115, 79,125,123,143,180,172, 34,123, 79,129,125, 82,174,
119,123,174, 26,194, 46,125,123, 81, 81,192,187,194,255,255,121, 80,136, 79,118,
 83, 44,148,108, 79,136,114, 80, 26,158, 79, 83,136, 80, 26, 44, 86, 79, 81,136,
 80,128, 26, 72, 83, 79, 77, 81, 26, 78,129,121, 80, 89,120,116, 26,187,115, 81,
116, 83,120,187, 80,162, 80, 89, 96, 81, 83,187,119,188,194,120,187, 80,116, 86,
 81,120,116,194, 80,144,101, 80, 81,120, 83, 26,188, 81,121, 60, 67, 53,174, 74,
 83,115,129,125, 60, 80,143,123, 10,129,125, 80, 82,123, 74,119,123,125,174,143,
 74, 46, 72, 60, 74, 53, 67, 79,123,  2, 60,129, 82, 80,123,143, 83,115,129, 80,
 62, 26,123,125,121, 62,123,125, 79, 26,143, 10,129, 80, 82,123,125, 76,119,174,
 62, 81, 80, 46,194, 81, 81,192,194,255,255,255, 20, 80, 79,129, 81,125, 69, 26,
 30, 38,194,193,179,255,255,121, 75,129,125, 79, 80, 83, 81, 81, 32,185,192,194,
255, 32, 75, 83,125, 20, 18, 89, 44, 75, 38, 79,129, 83,125, 86, 75, 80, 89,143,
192, 79, 65,123,121,136, 61, 77, 80, 79, 81, 67, 81, 79, 78, 30, 83, 80, 66, 79,
 83,136, 78, 30, 81,115, 79, 61, 77,136, 80, 83, 72, 61, 79, 77, 81, 78,136, 69,
136, 77, 79, 61, 83, 89,129, 69, 68, 75, 61, 54, 80, 22,121, 68, 54, 32, 75, 22,
120, 66,120, 75, 80, 44, 46, 20,162, 68, 75, 96,114, 80, 81,101, 80, 68, 96, 75,
 83, 81, 63,116,120, 68, 54, 75, 80, 79,121, 77,123, 32, 86, 44,129,119,123,194,
 81, 72, 86, 78, 72,129,123,125, 72, 32,158, 18,129,123, 83, 77, 32, 82, 26,129,
123, 32,174,125, 72,  2,129,123, 32, 83,174,194, 81,121, 77, 74,123, 79,129, 80,
 79,129,125, 80, 82,123, 74,115,129, 77, 30,180, 79, 78,119,123, 74,125, 78, 77,
 30, 86, 80,123,129,125, 77, 74, 67,123,125, 74, 30, 22, 46, 75,121,129, 74,125,
 73, 26, 68,115,129, 72, 73,143, 71, 76, 67, 32,192,194, 74,123,178, 79, 72,129,
 82, 46,194, 73, 16,129,125, 73, 74, 61,123, 93, 72,129,143,154, 74, 76, 80,121,
 77,123, 73, 87, 30, 79, 67,123, 81, 32,192,194,255, 16,123,125, 87, 73, 81, 83,
115, 77,125,123,129, 87, 73, 72, 73,125, 79, 87,129, 30, 18,123, 73, 83, 30, 87,
129,101,129,116,120, 80, 75, 81, 26, 83,114,116,120, 80, 81, 83, 75,162, 75, 96,
 81, 80, 68, 54, 72, 80, 68, 96, 75, 83, 81, 26, 80, 81,120, 83, 26,188, 34, 81,
 80, 75,120, 61, 68,123,155,136, 80, 81, 61,118, 46, 72, 77, 61, 79,136, 81, 83,
114, 80, 79,138,136, 68, 46, 65,136, 61, 81, 77, 79, 78,116, 79,136, 61, 80, 75,
 83, 86,136, 77, 79, 81, 78, 83, 79,114,129, 32, 77,194,125,123, 16, 32,123,129,
 65, 81, 82,164,123,129, 32, 80, 81,125,148, 78,129,123, 32, 30, 77,155,180, 78,
143, 80, 30, 77,255,255,255,255,255,255,255, 80,116,123, 79, 87,125, 20, 44,114,
123,125, 26,143, 20, 44, 93,123, 87,129,125,143, 82, 28,123, 79,129,125, 87, 81,
 72, 73, 79, 77,129, 81,125, 78,123, 79, 87,129,125, 81,125,114,116, 54, 80,194,
 81, 79, 16,116, 80, 54, 75, 96, 26,155, 80, 75,130,188, 79, 83, 72, 77, 79, 30,
 75, 78, 96, 34, 61, 75,130, 80, 81, 26,148, 75,116,130, 80, 79, 96, 75,106, 72,
129, 82, 46,194, 73,114, 72, 71, 32, 74,129, 46,116,123,129, 74, 26, 46,194,110,
 72, 82,129, 46, 73, 44, 65, 73,129, 72, 26, 71,123, 86,129, 72, 61,143, 26,125,
123,115,136,149, 37, 43,123, 17, 41, 80,148, 17, 80,174,187, 82,121,155,121,187,
 80,123, 74, 31, 18,187, 80,121, 33, 82,188, 78, 80, 79,121,194, 82,187,168, 37,
 74,188, 31,187, 80, 80, 16,136, 20,118,132, 73, 32, 72,180,188, 73,187, 77, 20,
124,180, 26,179,187,194,193, 22, 79, 73,136,187,174,180,168,174,188, 73,114, 20,
 82,155, 32,174,110, 66,118,132, 83, 81,187, 81,192,194,255,255,124, 26,193,194,
187,192,255,155, 80, 20,132, 82, 69, 62, 78,136, 79, 80,187,128, 62,168, 80, 82,
114,132, 20, 76,  4, 62, 80,132,188,136, 82, 82,124, 26, 75,194,193,187,192, 78,
136, 79, 30, 80, 26, 20, 81, 81,187,192,194,255,255,155,110, 80, 32, 20, 79, 75,
168, 80, 20, 26, 75,136,114, 72,188, 77, 81, 78, 22, 20,138, 18, 80,143,123,125,
 46, 74,124,194,193,192,255,255,255, 72,123, 74, 79, 78, 77, 83, 78, 79, 80,129,
123, 74,180, 77, 79, 80, 78, 46,123, 82,168,129, 74, 82,143, 44, 46,187, 16,136,
132,110, 44,166, 46, 80,195,152,136, 30, 83, 80, 93,180,136,128, 30, 46, 79,154,
 32, 80,136, 46,188, 74, 72,152, 79,180, 77, 74, 46, 86,152,136, 32,180,166, 79,
 72, 30,115,128,180,193,187, 74, 38, 71,180, 80,128, 79,174,194, 73,184,188,128,
 80,187,180, 74,130,112,116, 24, 79, 22,  4,128,194,136,180,187,184, 28,194,128,
182,187,180,184,180,101,136, 78, 77,188,194, 79,115, 77, 32, 79, 30, 22,136, 16,
136, 77, 30, 80, 79,194, 22, 80,136, 79, 32, 77, 78,121, 30, 79,188,136, 32,159,
 79,136, 80,152,188, 30,174, 79,115,180,128,194,187,188, 77, 74, 32,180,130, 81,
192,178, 16, 30, 83, 81, 20, 26,188,  4, 72,180,188, 82, 20,128, 22, 72,180,188,
174,128, 20, 28, 72,180,128,188, 83, 20, 77, 74, 32,130, 81,178,194,187, 16,180,
 83,188, 79, 81, 63,119,188,180,136, 20, 81, 30, 22,188,180, 79,136, 22, 82, 18,
188,180, 83, 82,136, 63, 26,180, 63, 83, 79,188, 91,194, 16,188,138,136, 30,180,
187, 93, 95,188,187, 46, 44,255,115,152, 79, 32,180,187, 95, 22,180,136,166,152,
195, 95,121,188,159,187,180,136,166, 74,130,112,136,178,116, 14, 46,115,180,187,
 30, 38, 22, 40,121, 38, 22, 78, 79, 30, 80, 18, 30, 82, 83, 38,188,136, 26,180,
 30,194, 79, 40, 38, 16, 30, 38, 80, 14, 40, 34, 79, 80, 38,194,188,180,136,101,
136,155, 74, 80,121, 31, 82,123,163, 80, 82, 31,121,123, 37,149, 74, 37, 17, 31,
123, 19,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
255,255,255, 80,114,174,188, 20, 46,118,180,155, 32,136,188, 26,192,110,154, 87,
114,136, 79,138,110,116,136, 79,188, 82, 83,138,149, 79, 73, 32, 83, 26,188, 72,
 73,136,188, 77, 30, 79,194,114,136,180,152,118,138,188, 16,136,188,195,138,152,
187, 93,188, 46, 95,187, 44,255,116,136, 79,188, 30,187,195,106,136, 79,152,188,
 30,180, 65,136, 30, 95,188,138,180, 83,155, 80, 82, 32, 79, 81,132,116,136, 69,
 82, 97, 80,188, 72, 69, 77,188, 79, 82, 55,106,136, 80, 62, 82, 79, 81,  4,136,
 80, 82, 62, 69, 76, 18,136, 80, 62, 82, 81,188,188,114,136, 80,160,128, 74,174,
 80, 80, 30, 32,194,136,195, 86,136, 79, 80, 32,194,160,116,136, 79, 80, 32, 95,
 83,106,136, 80, 79,153,128,160,155, 32, 74, 80,110,138,118, 82,155,136, 80, 81,
 61,118, 46, 72, 77, 61, 79,136, 81, 83,114, 80, 79,138,136, 68, 46, 65,136, 61,
 81, 77, 79, 78,116, 79,136, 61, 80, 75, 83, 86,136, 77, 79, 81, 78, 83, 78, 79,
 79, 71,136, 77,188,174, 64,121,136,174,188, 20, 80, 82, 16, 65,174,188,128,114,
 82,148,108,114,136, 65, 30, 82,164,128, 30,110, 65, 83, 81,150,110, 65, 30, 83,
128,114,187, 16,166, 30,128,136,188,194,121,136,188,159,166,152, 88, 80,195,194,
136,152,132,128,115, 79, 30, 80, 74,136, 32, 79,136,152,188,180, 79,194,  4, 79,
 30,136,128, 95,195,180,121, 30,188,136,159,187,166,101,152,136,174, 79,188, 30,
 80,136,194,188,152,174,192, 22,152,166, 80,187,136, 30, 16, 30,136,166, 74, 32,
152, 79,136, 80,152,188, 30,174, 30,115,180,187,128, 80,184, 82, 71,180, 80,128,
 79,174,194, 85,180,194,192, 38,178,136, 22,128, 82, 83, 80,136,174, 28,128,174,
 82,180, 79, 80, 26,174,180, 80,128,182,192,174,115, 32, 79, 80,136,160,128, 80,
195,136,188, 95, 82,192,121, 79, 80,136,188,160, 30, 81, 79,136,160,188,166, 32,
  4, 79, 32, 30,136, 80, 95, 22, 32, 79, 95,136, 30,166,188, 80,195,194, 80,136,
 32,174, 22, 79,136, 32, 80, 83, 95,121,136, 79, 80, 30,187,160,115,128, 80, 79,
 32,136,160, 81, 79,136,160, 77, 80, 71, 71, 79, 77, 78, 95,136, 83,121,136,162,
 31,188, 80,121, 82,187,163, 80,187,123, 82,121, 43,149,121, 80, 82,188,123, 83,
150, 19,121, 80, 83, 82,187,155,121, 80,187, 74, 33, 83,255,255,255,255,255,255,
255,187, 80,136, 30, 88, 95,195,166, 18,188,136, 80, 32,132, 44, 72,195,194,188,
180, 30, 46, 16,152,136,188,166, 44, 46, 77,136,188,166, 95,152, 88, 93,136, 95,
159,128, 30, 22,188, 80,136,187,195, 30, 95, 44, 18, 80,136, 46,187, 83, 74, 72,
136, 30, 32, 80, 74, 83, 86, 79,136,160, 88,167,174,148,136,187, 46, 80,195,167,
149, 74, 82,118, 46, 80,136, 80,154,136, 87,187,114,110,118, 72, 79,180, 30,128,
193,179,149,118, 20, 44,174,187,138,155,136,174,110, 32,188,138,168, 87,136, 32,
187, 26, 44, 18,174,136,188, 59,187, 87,174, 80,136,188,195, 30,166,128, 93,136,
 95,158, 80, 32, 46,116,166,167,173,181,255,255, 72,195, 74,180,158,188,160,155,
 80,160,188, 74,158,166, 16,136, 80,188,167, 82,166,194, 18,188,187,136,152, 46,
195,136,188,187,136,195,193,255,108, 88,188,195,187,193, 80, 16,152,136,187,188,
166,195, 93,187, 46,188, 95, 44,255, 72,136,195, 46,180,187, 88, 77, 79,121,136,
174,188, 20, 78, 82, 79,136, 63, 49, 56, 20,188, 16, 65,174,188, 20, 82,136,164,
128, 30,110, 65, 72, 83,148,108,114,136, 65, 30, 83,150,110, 65, 30, 72, 83,114,
174, 80,195,136,188, 95, 82,192,121, 79, 80, 78,136,188, 30, 81, 79,188,166, 32,
136, 46, 22,136, 79, 32, 78, 30, 80,115, 79, 80, 78,136, 32,195,  4,136, 30, 32,
 79, 80, 78,188, 80,195,194, 80,136, 32,174,121,136, 79, 80,187, 78,174,  4, 79,
136, 78, 80, 95,174, 22, 79,136, 80, 83, 95, 77, 81, 79, 91, 84, 63, 78, 80, 70,
 79,136, 78, 77, 80, 95,180,121, 30,188,136,187,174,166,115,128, 30,166, 78,187,
110,101,152,136, 79,174,194,188, 80,136,194,188,152,174,192, 16,136, 80, 78, 30,
 79,188,  4,174, 79, 78,187, 30,188,187, 16,188,166,136, 30,194, 22,121,136,188,
166, 95,152, 88, 80,195,194,136,152,132,128,115, 79, 80, 30, 32, 78,166,101, 79,
136,152,195,188,194,119,152,136,166,188,180,194, 80,115,188,187,136, 30,110, 26,
121,136,174, 79,188, 78, 82,119, 73,136,174, 81,188, 30, 70, 79,136, 73,188,180,
 87, 84,174, 79,180,136, 77, 78, 63,136, 73, 79, 87,174, 30, 74,115, 72,162, 79,
 20,143,125, 44, 26,150,129, 79, 74,123,125, 44, 16,129,125, 20, 79, 32, 74, 72,
 20,125, 86,180,158, 26,155,129, 71, 20, 74, 79,123,148,129, 71,158, 26, 79,123,
129,128,194,193,180,179,187, 30, 72,120,116,187,180, 32, 30, 18, 71, 73, 72,120,
 26, 67,124, 26,124, 75,180,179,193,  4,120,116,187, 73, 71, 32, 28, 67,187,116,
 73, 26, 60, 81, 18, 80,143,123,125, 46, 74,124,194,193,192,255,255,255, 72,123,
 74, 79, 78, 77, 83, 78, 79, 80,129,123, 74,180, 77, 79, 80, 78, 46,123, 82,168,
129, 74, 82,143, 44, 46,125, 81, 81,130,192,187,194,255,128,194,180,187,193,179,
 30, 18, 72, 81, 32,188, 26, 76, 72,116,187,180, 32, 30,110, 26,116, 60, 81, 72,
 67, 53, 86,116, 72,180,130, 81, 76, 20, 83, 72, 81, 32,183,182,123,128,180,179,
194,193,255,255, 78, 72, 32,129,143, 71, 38,124, 26,194, 75,193,185,192, 86,180,
 38,192,194,193,178, 72, 32,180, 38, 73, 12,125, 67, 81, 81,192,194,255,255,255,
124, 26,194,193,192,255,255,128,194,180,179, 30,193,255, 83,129, 32,172,143,123,
125, 78,123,129,172,143, 74,174, 77,123,143,172,129, 74,174,119,129,128,180, 30,
179,193,194,187, 22,116, 60, 72,174, 71,188, 28,116, 72,120, 73, 60,174, 72, 30,
 73,180,188,120,174, 18, 72,120,174, 26, 44, 46,155, 32,188, 71,174, 72, 76, 72,
162,123,125,143, 26, 46, 32, 72,194, 86,143, 80,193, 78, 16,129,123, 75, 26, 20,
 73,114,123, 32, 79,125,129, 71,155,129, 71, 32, 79, 75,194,164,123,125, 75, 76,
143, 71, 32, 16,174, 81,194, 60, 73,180, 72,180,174, 81,194, 38, 72,114, 72, 81,
 67, 24, 20, 44,106, 72, 81,180, 76, 71, 38, 65, 81, 72,194, 38,174,180, 18,180,
 72,194, 81,174, 44,194,114, 46, 32, 81, 73, 67, 88,106, 72, 46, 75, 73, 26, 71,
 72, 32, 72, 46, 30, 14, 22, 81, 73, 81, 22, 14, 71, 32,134, 46, 71, 75, 73, 26,
 88,128, 46,188, 44, 73, 71, 72,143, 83, 46, 75, 76, 72, 14, 22, 72,138, 73,180,
 30, 46, 14,114,128, 20, 44,134, 46,138, 78, 30, 76, 72, 81, 20, 46, 65,138, 22,
 67, 20,128, 14, 77,128, 81, 76, 46, 72, 22, 46, 83,143, 81, 72,194,123, 14,114,
129,125,194, 73, 72, 71, 72,194,174,129, 38, 73, 75,128,194,180,178,185, 30,179,
 18,194,129, 72, 73, 81, 71, 86,194, 38,180,179,178,192, 72,129,115,120,116,187,
180, 32, 30, 16, 30, 73, 67,120, 81,180,162, 73, 67,187,188,194,120,  2, 67, 76,
 73, 30, 60,116, 18, 73,120, 67, 75,180, 22, 28, 30, 76, 73, 60,120,116, 73,115,
180, 30,123,129, 32,143, 79,129, 72, 80,123,158, 87,148,123,129, 75, 30, 76, 80,
  2,129,123, 72, 30, 87, 74,119,123,129,125,174, 74,143, 16,129,123,125, 74, 30,
 76,125,115,116,187,180, 32, 30,110,148, 76,116, 30, 67, 60, 53,  2, 76,116,138,
188, 60, 30, 18, 76, 73,188, 75, 81,138, 73,116, 67, 60, 14, 20, 46,119, 73,116,
 67, 53,138,188,174,115,129,188,180,195, 32, 73, 73,195,188, 26, 46, 32, 75,162,
188,180,192,195,160,158,101,129,188,195,160,166, 30, 93, 32,153,188,160,166,143,
 71, 26, 32, 46,123, 73, 22, 46,115,129,180, 73, 38, 30, 14, 86,194, 38,178,180,
179,192,121, 72, 73,129, 38,194, 22, 73,129,194,125, 67,174, 76, 16,129, 38,143,
180, 67,125, 18,129,194, 75,174, 38, 73, 32,115,129,180, 72,143, 73,125, 73,180,
 81, 73, 20, 76,194, 86,180, 38,194,192,193,178,121, 81,180, 72, 38, 44,174, 16,
 38,194,192, 81, 26, 44,119,180,174, 81,194, 38, 72, 83,129, 90,138,179,180,193,
194,192, 22, 73, 75, 76,138, 67, 26, 28, 67, 71, 73, 76, 60, 75,115,116,120, 72,
187, 67, 71, 18, 72, 73, 71, 76, 22, 26,121, 72,120, 20, 46, 44,116, 32,121, 81,
 72,184, 20, 26,182,115,183, 72,185, 67, 60, 71,119, 81,194,182, 60, 72, 67, 16,
 81, 60, 26, 67, 75,184,101, 81, 24, 72, 71, 76, 73,  2, 60, 67, 81, 72, 71, 75,
 72, 16, 79, 75, 74,123, 76, 26,162, 44,123, 26,125,143,172,148, 32, 75, 79, 30,
 22,123,164,123, 79, 93, 74, 30, 44,150,123, 75,129, 76, 74, 32,255,255,255,255,
255,255,255,125,119, 72,116, 76, 81, 75, 67, 90,138,178,194,180,179,193, 97, 71,
 46, 72, 14, 76, 20, 18, 76, 26, 71, 46, 72, 22,101, 75,138, 46, 81, 14, 76,  2,
 60, 22, 76, 75, 14, 26, 22,121, 72,184,129,125, 30,143,115, 71,183,143,182, 72,
 14, 79, 71, 72,125, 73,129, 14, 22, 26,  4, 60, 18, 73,123, 78,182, 71, 72,125,
 30,129, 90,154, 71, 60,194, 73, 72, 26,115, 72, 32, 75,123, 81,182,119, 75,194,
 32, 72, 34,123, 79, 72, 71,125, 38, 20, 32, 26, 22, 28, 18,  4, 60, 73,101, 75,
 32,182, 38, 72, 20, 78,129,125, 72, 75, 71, 32,121,129, 72, 30,180,193,179,194,
187, 16, 73, 67, 76, 72,120,116, 18, 72, 73, 75,120, 44, 46,108,174, 72,193,180,
 44, 46,155, 72, 32, 67,174,116, 60,149, 53, 72, 75,188,174,187, 72,114,123, 32,
 79,125,129, 71, 16,129,123, 79, 74, 20, 73, 72,123,129, 32,194,125, 77,164,123,
 74, 79, 71, 70, 51,148, 71,123,129, 79, 32, 26,155, 71,123,129,172, 79, 20, 20,
 86,180,194, 38,178,192,193,155, 81,125, 72, 32, 60,123,154, 73,129, 26, 32, 44,
143, 81, 32, 81,192,194,255,255, 77, 72,129, 73, 44,143,125, 78, 72,129, 44,143,
 73, 26, 32, 81, 81,192,194,255,255,255, 86,180, 38,194,193,192,178,154,143,129,
 20, 26, 44, 67, 18, 72, 81, 26, 20, 44,174,162, 67,129, 44, 60,143,125, 93, 81,
 72,180, 38, 20,  0, 26, 86,180, 38,194,193,178,192,155,123, 34, 32, 60, 72,129,
154, 73,129, 20, 32, 44,143,162, 67,129, 44,174, 10, 73, 72,178,194,180,193,179,
185, 26,  4, 22, 28, 18, 72, 73, 44, 86,180,192,194, 38,193,178, 81, 32, 81,194,
192,255,255,155, 75, 81, 32, 60, 38, 67, 77, 72,129,143, 20,125, 36, 22, 72,129,
 32,143, 20, 26, 78, 72,129,143, 20,125, 36, 65,129,121, 72, 30, 26, 22, 44,116,
 72, 30, 73, 67,116,120, 53, 63, 72,120,116, 71, 76, 67, 67,120,116, 22, 26, 46,
 20, 68,120, 26, 72, 22,116, 75, 69, 72,116, 76, 75, 46, 73, 72, 16,129,123, 73,
 74, 75, 32,119,129, 30,123,194, 32, 74,115,129,123,193, 32,192,125,162,123, 30,
 26, 44, 32,174, 18,123,129, 32, 30, 73, 76, 34,123,129, 32, 26,143, 44, 67,115,
129,180,125,123, 30, 60,119,123,129,125, 74, 81,174, 66,123, 74, 81, 60,194, 32,
 64,123,125, 74, 22, 81,143,121,129, 74,143, 68,123, 69, 72,129,125, 74, 63,123,
 81, 73, 67,123, 32,192,194, 74,178, 79,129, 72, 80,123,158, 87,121,123, 26, 74,
125,129, 30,115,129,125,123, 30, 72, 32,119,123,129, 74,194,125, 30, 69,125, 32,
 22, 46,129, 76,123, 66, 82, 68, 75, 96, 90,138, 67, 82, 54,193,138, 96, 30, 68,
 82,138,193,136, 75, 96,121, 61,138,136, 68, 82, 54, 69, 61,136, 30, 26, 46, 75,
 26, 90, 75,138,180,136, 26,125, 72,116, 30, 73, 67, 53, 60,121,116, 72,138, 73,
 26, 30, 66,116, 20,130,192, 75, 32, 18, 76, 72, 67, 73,188, 22, 68, 26,116, 22,
 75,138,188, 34,116, 76, 73, 60, 53, 72
 };

unsigned char entries_2x26_6plies[702] = {
121, 77, 74,123, 79,129,125, 67, 80, 14, 30, 83, 22, 44, 53, 46, 60,174, 78, 32,
 82,143,194,193,192,178,255, 79,129,125, 80, 82,123,174, 74, 44, 46,143, 32, 83,
 53, 60, 67,154,194,193,192,255,255,255,255,255,255,255,115,129, 77, 30,180, 79,
123, 78,125, 74, 53, 67, 60, 83,143, 46, 80, 82, 44, 32, 14, 22,193,194,178,174,
192,119,123, 74,125, 77, 78, 80, 82, 30, 46,143, 67, 44, 14, 22, 83, 53,129, 60,
174,194, 32,193, 79,178,192,255, 86, 80,123,125, 77,129, 79,174, 74, 78, 53, 60,
 67, 83,143, 82,154, 32,194,193,178,192,255,255,255,255,255, 67,123,125, 74, 30,
 22, 46, 14,143,129, 32,192,194,174,154,193, 44,178,255,255,255,255,255,255,255,
255,255, 18,129, 83,123, 77, 82, 46, 30, 74, 79, 32, 80,125,143, 78, 67, 14, 22,
 44,174,154,194,193,178,192,255,255, 72,129,123, 79, 74, 77, 30, 32,125, 53, 60,
 67, 83,143, 46, 78, 14, 82, 80, 44, 22,194,193,178,192,255,255, 66,123, 79, 32,
 74, 30,129,125, 60, 78, 77, 83, 14, 80, 46,192,193, 53,174, 82, 67,143,194, 22,
180, 44,154, 63, 67,125,123, 77,129, 79, 32, 83, 82, 80, 74, 78,143, 53, 60, 30,
174, 22, 46,194,178,193,192, 44,255,255, 16,129,125, 30,123, 74, 60, 67, 83, 46,
 32, 44, 53,193, 14, 22, 78, 77, 79,143, 80, 82,192,194,174,154,255, 64,123,129,
 79, 80,125, 74, 83, 22, 32, 82, 46, 77, 53, 60, 67,143, 78,174, 30, 14,194,178,
192,193, 44,255, 69,125, 32, 30, 83, 14, 78, 79, 80, 46, 22, 77, 74,123,192, 67,
129, 53, 60, 82,178,194,154,143, 44,193,255, 68, 79, 74, 32,123, 77,125, 14,193,
 30, 80, 83, 22, 78,192,129, 53, 46, 82, 60, 67,143,194,154,178,174, 44,  2, 60,
 67,123, 83,129, 53, 74, 30, 79,125,143, 46,194, 82, 78, 77, 44, 14, 80, 22,193,
 32,174,178,154,192, 93, 79, 74,129,143, 77, 80, 67,123,125, 83, 82, 53, 60, 78,
 46, 30,174, 14, 22, 32,154,178,255,255,255,255,101,123, 79, 60,129, 82, 80, 83,
 67,125, 74,143, 32,194, 46, 78, 77, 53, 22, 14, 30, 44,193,178,192,174,255,  4,
 60,123, 67,129,125, 83, 74, 30, 77, 79, 32,193, 82, 44, 46, 78, 22,143, 14, 80,
 53,194,154,178,174,192, 34, 60,129,123, 53, 67,125, 74, 77,174, 79, 83,143, 80,
 32,154, 30,194, 82, 46, 14, 44, 22, 78,193,178,192,168,143, 30,129, 74,123, 32,
 53, 44, 46,125, 67, 80, 79, 83, 60, 82,194,180,193, 22, 14, 78, 77,178,255,255,
 26, 60,129,125, 74, 83, 30,143, 53, 67,174, 79,123, 77, 46,154, 44, 14, 78, 80,
 22, 32, 82,194,193,178,192,188, 32, 46, 74, 82, 53, 67, 80, 78, 77, 30, 83, 44,
 79, 60, 22, 14,192,193,154,178,255,255,255,255,255,255,148,123,129, 53, 74, 80,
 79, 60, 67, 30,192, 83, 82,125, 46,194,143, 32, 77, 44, 78, 14, 22,178,174,193,
255,149, 74,123, 67, 53, 83,125, 82, 60,143,129, 80, 77, 46, 78, 22, 32, 79, 14,
194, 30, 44,180,154,178,193,174,164, 67,123,125, 80, 30, 74,143,129, 53, 32, 79,
 83, 60, 82,192,154, 46, 77, 44, 22, 14,193,194,174, 78,255,150, 53,123, 30, 67,
125,129, 83, 79, 32, 82, 14, 22, 46, 44, 78, 77, 80,193, 74, 60,143,180,194,192,
178,154 };

/* Specification of opening books to be used. Order matters!
   The first matching move is selected, so more accurate books
   should be placed at the beginning of the array! */
const book_t books[BOOKS] = {
	{
		"4x12 8-plies",
		4,		/* book depth */
		12,		/* book branching factor */
		22620,	/* total size */
		entries_4x12_8plies
	},
	{
		"6x6 6-plies",
		6,		/* book depth */
		6,		/* book branching factor */
		55986,	/* total size */
		entries_6x6_6plies,
	},
	{
		"2x26 6-plies",
		2,		/* book depth */
		26,		/* book branching factor */
		702,	/* total size */
		entries_2x26_6plies
	}
};

#endif
