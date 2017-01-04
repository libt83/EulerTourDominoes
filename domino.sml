(*
		Brandon Semba
		Assignment 8
		03/02/2016
*)

(* 
	Creates an N-N set of dominoes
	By using an inner helper function to start
	the recursive list of domino tuples by starting 
	the (N,N) tile and recursing down to the (0,0) tile.
*)
fun dominoes(N: int) =
	     let
		 (*
	             Starts at the highest tile in the set and
	             recursively decrements down till it reaches the base case tile of (0,0)
		 *)
	         fun create(x : int, y : int) =
				case (x,y) of
		        	     (0,0) => [(0,0)]
				     |   _ => if y = 0 then (x,y)::create(x-1,x-1)
		        			       else (x,y)::create(x, y-1)		
		in
	            create(N, N)
		end
		
(*
   This function takes in an N-N set of dominoes as a list and then
   utilizes Fleury's algorithm to generate an Euler circuit using all the tiles
   from within the domino set.
*)  
fun Eulers(L :(int*int) list) =
	  let
	  (*
	      Helper that uses local helper functions to recursively build the
	      Euler circuit using all the dominoes in the set.
	   *)
	      fun buildCircuit(T: int*int, L: (int*int) list) =
	          let
		      (* Used by exists to check domino set for path from current tile to 1st node *)
		      fun nodeTo1stNode(tile: int*int) =
		          case tile of
			       tile => if #2 T = #1 tile then true else false
		      (* Used by exists to check domino set for path from current tile to 2nd node *)
		      fun nodeTo2ndNode(tile: int*int) =
			  case tile of
			       tile => if #2 T = #2 tile then true else false
		      (* Used by filter to get a list of tiles with matching 1st nodes to the current tile node. *)
		      fun getPathsTo1stNode(tile: int*int) =
			  case tile of
			       tile => if #1 tile = #2 T then true else false
		      (* Used by filter to get a list of tiles with matching 2nd nodes to the current tile node. *) 
		      fun getPathsTo2ndNode(tile: int*int) =
		          case tile of
			       tile => if #2 tile = #2 T then true else false 
		      (* Used by map to flip tiles in the filtered set. *)
		      fun tileFlip(tile: int*int) =
			  case tile of
			       tile => (#2 tile, #1 tile)
		      (* Removes a tile from the set of dominoes. *)
		      fun removeTile(tile: int*int, L: (int*int) list) =
			  case L of
			       [] => []
		           | h::t => if #1 tile = #2 h andalso #2 tile = #1 h then removeTile(tile,t) else (
								              if h <> tile then h::removeTile(tile,t) else removeTile(tile,t))
		      in
		          let
			      val dominoSet = removeTile(T,L)
			      (* Stores the current list of paths available from current node. *)
			      val availablePaths = if List.exists nodeTo1stNode dominoSet then List.filter getPathsTo1stNode dominoSet else (
				                   if List.exists nodeTo2ndNode dominoSet then List.map tileFlip (List.filter getPathsTo2ndNode dominoSet) else [])
			      val tile = T
			  in
			      case availablePaths of
			           [] => [T]
			       | h::t => T::buildCircuit(h,dominoSet)
			  end
		      end
		in
			 buildCircuit(hd L, L)	 		     
		end
		
(*
	Goes through the list and checks to see if tuples of
	dominoes are joined together by matching values.
	It checks each domino and its adjacent domino to see if 
	the first domino's right value matches the second domino's left
	value and if they do not match then either the first or the second
	domino's values are swapped in order to maintain their connection.
*)			
fun flip(L: (int*int) list) =
		let
		    fun switchTail(tail: (int*int) list) = (#2 (hd tail), #1 (hd tail))::tl(tail)
		in
		  case L of
		        [] => []
		  |  h::[] => h::[]
		  |  h::t => if #2 h <> #1 (hd t) andalso #1 h = #1 (hd t) then (#2 h,#1 h)::flip(t) else
			    					 (if #2 h <> #1 (hd t) andalso #2 h = #2 (hd t)
			    					 then h::flip(switchTail(t)) else h::flip(t))
		end
		
(*
	Takes an int*int list representing domino tiles and
	converts the list of tuples into a single string 
	representation of the domino tiles.
*)
fun listAsString(L : (int*int) list) =
		let
		(*Helper function to convert (int*int) list -> string list*)
		    fun convert(t: (int*int)) =
		        case t of
			    t => "(" ^ (Int.toString(#1 t) ^ "," ^ Int.toString(#2 t) ^ ")")
		in
		    let 
		        val list = map convert L
		    in	(*Folds the string list elements into a single string*)
		        if L <> [] then "[" ^ foldl(fn(a,b) => b ^ a) (hd list) (tl list) ^ "]" else "[ ]"
		    end			
		end										
		
(*
		Generates a solution to the puzzle based on the integer value of N,
		which is the highest dot value for the set of dominoes.
		If N is an odd integer then an empty list is returned, because
		there is no Euler circuit present in odd domino sets.
*)		
fun solution(N: int) =
	     case N of
	         N => if N mod 2 = 1 then [] else Eulers(dominoes(N))

(*
		The driver for the program. It takes two functions: listAsString and solution.
		It also takes an integer value for N, with N being the highest dot value within
		the domino set and then produces a string representation of an Euler circuit found
		within the domino set.
*)
fun driver(F1,F2) N =
	   F1(F2 N)
		
						
						
